#include "backtrace.h"

#include <bfd.h>
#include <cxxabi.h>
#include <dlfcn.h>
#include <execinfo.h>
#include <link.h>
#include <unistd.h>

#include <cassert>
#include <climits>
#include <cmath>
#include <csignal>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <exception>
#include <string>
#include <unordered_map>
#include <vector>

static struct dbfd
{
  void (*init)(void);
  bfd *(*openr)(char const *filename, char const *target);
  bfd_boolean (*check_format)(bfd *abfd, bfd_format format);
  bfd_boolean (*check_format_matches)(bfd *abfd, bfd_format format, char ***matching);
  bfd_boolean (*close)(bfd *abfd);
  bfd_boolean (*map_over_sections)(bfd *abfd, void (*func)(bfd *abfd, asection *sect, void *obj), void *obj);
} dbfd;


struct bfd_cache
{
  ~bfd_cache ()
  {
    for (auto pair : cache)
      if (pair.second)
        dbfd.close (pair.second);
  }

  bfd *operator () (char const *file_name)
  {
    auto found = cache.find (file_name);
    if (found != cache.end ())
      return found->second;
    return cache[file_name] = dbfd.openr (file_name, nullptr);
  }

  std::unordered_map<char const *, bfd *> cache;
};


// Read in the symbol table.
static asymbol **
read_symtab (bfd *abfd)
{
  asymbol **syms;

  long symcount;
  unsigned int size;

  if ((bfd_get_file_flags (abfd) & HAS_SYMS) == 0)
    return 0;

  symcount = bfd_read_minisymbols (abfd, false, (PTR *)&syms, &size);
  if (symcount == 0)
    symcount = bfd_read_minisymbols (abfd, true /* dynamic */,
                                     (PTR *)&syms, &size);

  if (symcount < 0)
    throw std::runtime_error (bfd_get_filename (abfd));

  return syms;
}

// This struct is used to pass information between
// location_of_addr and find_address_in_section.
struct env
{
  bfd_vma      pc;
  char const  *file;
  char const  *func;
  unsigned int line;
  bool         found;
  asymbol    **syms;
};

// Look for an address in a section.  This is called via
// bfd_map_over_sections.
static void
find_address_in_section (bfd *abfd,
                         asection *section,
                         void *data)
{
  struct env *env = static_cast<struct env *> (data);

  if (env->found)
    return;

  if ((bfd_get_section_flags (abfd, section) & SEC_ALLOC) == 0)
    return;

  bfd_vma vma = bfd_get_section_vma (abfd, section);
  if (env->pc < vma)
    return;

  bfd_size_type size = bfd_section_size (abfd, section);
  if (env->pc >= vma + size)
    return;

  env->found = bfd_find_nearest_line (abfd, section, env->syms, env->pc - vma,
                                      &env->file, &env->func, &env->line);
}


static std::string
demangle (char const *name)
{
  size_t length;
  int status;
  char *demangled = abi::__cxa_demangle (name,
                                         nullptr,
                                         &length,
                                         &status);
  if (status != 0)
    return name;
  else
    {
      std::string result (demangled, length);
      free (demangled);
      return result;
    }
}


struct location
{
  std::string  file;
  unsigned int line;
  std::string  func;
};

static location
location_of_addr (bfd *abfd, bfd_vma addr, asymbol **syms, char const *origin)
{
  std::vector<location> addrs;

  struct env env;
  env.syms = syms;

  env.pc = addr;

  env.found = false;
  dbfd.map_over_sections (abfd, find_address_in_section, (PTR)&env);

  if (!env.found)
    {
      char name[11];
      snprintf (name, sizeof name, "0x%lx", (unsigned long)addr);
      return { "??", 0, name };
    }
  else
    {
      std::string name;
      if (env.func == nullptr || *env.func == '\0')
        name = "??";
      else
        name = demangle (env.func);

      if (env.file == nullptr)
        {
          char resolved_path[PATH_MAX];
          realpath (origin, resolved_path);
          char const *h = strrchr (resolved_path, '/');
          if (h != nullptr)
            env.file = h + 1;
          else
            env.file = "??";
        }
      else
        {
          char const *h = strrchr (env.file, '/');
          if (h != nullptr)
            env.file = h + 1;
        }

      return { env.file, env.line, name };
    }

}


static location
process_file (bfd_cache &cache_bfd, char const *file_name, bfd_vma addr)
{
  bfd *abfd;
  char **matching;

  abfd = cache_bfd (file_name);

  if (abfd == nullptr)
    throw std::runtime_error (file_name);

  if (dbfd.check_format (abfd, bfd_archive))
    throw std::runtime_error ("can not get addresses from archive");

  if (!dbfd.check_format_matches (abfd, bfd_object, &matching))
    throw std::runtime_error ("no matching format");

  asymbol **syms = read_symtab (abfd);

  location loc = location_of_addr (abfd, addr, syms, file_name);

  free (syms);

  return loc;
}

#define MAX_DEPTH 16

struct file_match
{
  char const *file;
  void *addr;
  void *base;
};

static int
find_matching_file (struct dl_phdr_info *info,
                    size_t size, void *data)
{
  struct file_match *match = static_cast<struct file_match *> (data);
  long n;

  const ElfW (Phdr) * phdr;
  ElfW (Addr) load_base = info->dlpi_addr;
  phdr = info->dlpi_phdr;
  for (n = info->dlpi_phnum; --n >= 0; phdr++)
    if (phdr->p_type == PT_LOAD)
      {
        ElfW (Addr) vaddr = phdr->p_vaddr + load_base;
        if ((ElfW (Addr)) match->addr >= vaddr &&
            (ElfW (Addr)) match->addr < vaddr + phdr->p_memsz)
          {
            // we found a match
            match->file = info->dlpi_name;
            match->base = reinterpret_cast<void *> (info->dlpi_addr);
          }
      }
  return 0;
}


static std::vector<location>
stacktrace ()
{
  assert (dbfd.init);

  std::vector<void *> buffer (100);
  int stack_depth = backtrace (&buffer[0], buffer.size ());
  // skip _start and __libc_start_main function
  buffer.resize (stack_depth - 2);
  // skip backtrace.cpp functions and std::terminate functions
  buffer.erase (buffer.begin (), buffer.begin () + 7);

  bfd_cache cache_bfd;

  std::vector<location> locations;
  for (void *address : buffer)
    {
      struct file_match match;
      match.addr = address;

      dl_iterate_phdr (find_matching_file, &match);
      bfd_vma addr = static_cast<char *> (address) - static_cast<char *> (match.base);

      location loc;
      if (match.file && match.file[0])
        loc = process_file (cache_bfd, match.file, addr);
      else
        loc = process_file (cache_bfd, "/proc/self/exe", addr);

      locations.push_back (loc);
    }

  return locations;
}


template<typename T>
static void dlsym (void *handle, char const *name, T *&target)
{
  target = (T *)dlsym (handle, name);
}

static bool
dbfd_init (void)
{
  assert (!dbfd.init);

  void *handle = dlopen ("libbfd.so", RTLD_NOW);
  if (!handle)
    return false;

  dlsym (handle, "bfd_init",                 dbfd.init);
  dlsym (handle, "bfd_openr",                dbfd.openr);
  dlsym (handle, "bfd_check_format",         dbfd.check_format);
  dlsym (handle, "bfd_check_format_matches", dbfd.check_format_matches);
  dlsym (handle, "bfd_close",                dbfd.close);
  dlsym (handle, "bfd_map_over_sections",    dbfd.map_over_sections);

  if (dbfd.init)
    {
      dbfd.init ();
      return true;
    }

  return false;
}


static void
print_backtrace ()
{
  std::vector<location> trace = stacktrace ();

  size_t max_width = 0;
  for (location loc : trace)
    max_width = std::max ( max_width
                         , 5 // 2 spaces + index (1-99) + 1 space
                         + (loc.line == 0
                            ? 0 // no line number
                            : 1 // : before line number
                              + int (ceil (log10 (loc.line))))
                         + loc.file.length ()
                         + 1 // space between location and function name
                         );
  assert ((int)max_width > 0);

  printf ("stack trace:\n");
  int i = 0;
  for (location loc : trace)
    {
      int width = printf ("  %-2d %s", ++i, loc.file.c_str ());
      if (loc.line != 0)
        width += printf (":%d", loc.line);
      assert (width <= (int)max_width);
      for (size_t i = 0; i < max_width - width; i++)
        fputc (' ', stdout);
      puts (loc.func.c_str ());
    }
  printf ("\n");
}


static void
terminate_handler (bool unexpected)
{
  char separator[81];
  memset (separator, '-', sizeof separator - 1);
  separator[sizeof separator - 1] = '\0';

  printf ("\n\n%s\n", separator);

  if (std::type_info const *const t = abi::__cxa_current_exception_type ())
    {
      std::string name = demangle (t->name ());
      printf ("terminated by ");
      if (unexpected)
        printf ("unexpected ");
      printf ("exception of type %s\n", name.c_str ());

      try { throw; }
      catch (std::exception const &e)
        {
          printf ("  what: %s\n", e.what ());
        }
      catch (char const *e)
        {
          printf ("  value: %s\n", e);
        }
      catch (int e)
        {
          printf ("  value: %d\n", e);
        }
      catch (...)
        {
        }
      printf ("%s\n", separator);
    }

  print_backtrace ();

  exit (254);
}


static void
fork_debug (int signum)
{
  if (int pid = fork ())
    {
      // parent
      assert (pid < 65536);
      char pidstr[8];
      snprintf (pidstr, sizeof pidstr, "%d", pid);

      char const *argv[] = {
        "sudo",
        "cgdb",
        "--",
        "--pid",
        pidstr,
        nullptr,
      };

      execvp (argv[0], const_cast<char **> (argv));
      perror ("execvp");
      exit (EXIT_FAILURE);
    }

  // wait for gdb to attach
  volatile bool resume = false;
  while (!resume)
    /* busy wait */;

  // child proceeds with fork_debug disabled
  signal (SIGTRAP, SIG_DFL);
}


void
backtrace_init ()
{
  if (dbfd_init ())
    {
      std::set_terminate  ([] { terminate_handler (false); });
      std::set_unexpected ([] { terminate_handler (true ); });
    }

  signal (SIGTRAP, fork_debug);
  signal (SIGABRT, [] (int) {
            print_backtrace ();
            signal (SIGABRT, SIG_DFL);
            raise (SIGABRT);
          });
}
