# 1 "sem-check.cx"
# 1 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 30 "/usr/include/stdc-predef.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4
# 31 "/usr/include/stdc-predef.h" 2 3 4
# 1 "<command-line>" 2
# 1 "sem-check.cx"
# 1 "../../include/ast.h" 1
       

# 1 "../../include/basics.h" 1
       

# 1 "../../include/sysdep.h" 1
       
# 4 "../../include/basics.h" 2

# 1 "/usr/include/assert.h" 1 3 4
# 36 "/usr/include/assert.h" 3 4
# 1 "/usr/include/features.h" 1 3 4
# 371 "/usr/include/features.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/sys/cdefs.h" 1 3 4
# 385 "/usr/include/x86_64-linux-gnu/sys/cdefs.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 386 "/usr/include/x86_64-linux-gnu/sys/cdefs.h" 2 3 4
# 372 "/usr/include/features.h" 2 3 4
# 395 "/usr/include/features.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/gnu/stubs.h" 1 3 4
# 10 "/usr/include/x86_64-linux-gnu/gnu/stubs.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/gnu/stubs-64.h" 1 3 4
# 11 "/usr/include/x86_64-linux-gnu/gnu/stubs.h" 2 3 4
# 396 "/usr/include/features.h" 2 3 4
# 37 "/usr/include/assert.h" 2 3 4
# 67 "/usr/include/assert.h" 3 4



extern void __assert_fail (const char *__assertion, const char *__file,
      unsigned int __line, const char *__function)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));


extern void __assert_perror_fail (int __errnum, const char *__file,
      unsigned int __line, const char *__function)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));




extern void __assert (const char *__assertion, const char *__file, int __line)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));



# 6 "../../include/basics.h" 2
# 1 "/usr/include/stdio.h" 1 3 4
# 29 "/usr/include/stdio.h" 3 4




# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h" 1 3 4
# 212 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h" 3 4
typedef long unsigned int size_t;
# 34 "/usr/include/stdio.h" 2 3 4

# 1 "/usr/include/x86_64-linux-gnu/bits/types.h" 1 3 4
# 27 "/usr/include/x86_64-linux-gnu/bits/types.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 28 "/usr/include/x86_64-linux-gnu/bits/types.h" 2 3 4


typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;


typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;

typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;







typedef long int __quad_t;
typedef unsigned long int __u_quad_t;
# 130 "/usr/include/x86_64-linux-gnu/bits/types.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/typesizes.h" 1 3 4
# 131 "/usr/include/x86_64-linux-gnu/bits/types.h" 2 3 4


typedef unsigned long int __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long int __ino_t;
typedef unsigned long int __ino64_t;
typedef unsigned int __mode_t;
typedef unsigned long int __nlink_t;
typedef long int __off_t;
typedef long int __off64_t;
typedef int __pid_t;
typedef struct { int __val[2]; } __fsid_t;
typedef long int __clock_t;
typedef unsigned long int __rlim_t;
typedef unsigned long int __rlim64_t;
typedef unsigned int __id_t;
typedef long int __time_t;
typedef unsigned int __useconds_t;
typedef long int __suseconds_t;

typedef int __daddr_t;
typedef int __key_t;


typedef int __clockid_t;


typedef void * __timer_t;


typedef long int __blksize_t;




typedef long int __blkcnt_t;
typedef long int __blkcnt64_t;


typedef unsigned long int __fsblkcnt_t;
typedef unsigned long int __fsblkcnt64_t;


typedef unsigned long int __fsfilcnt_t;
typedef unsigned long int __fsfilcnt64_t;


typedef long int __fsword_t;

typedef long int __ssize_t;


typedef long int __syscall_slong_t;

typedef unsigned long int __syscall_ulong_t;



typedef __off64_t __loff_t;
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;


typedef long int __intptr_t;


typedef unsigned int __socklen_t;
# 36 "/usr/include/stdio.h" 2 3 4
# 44 "/usr/include/stdio.h" 3 4
struct _IO_FILE;



typedef struct _IO_FILE FILE;





# 64 "/usr/include/stdio.h" 3 4
typedef struct _IO_FILE __FILE;
# 74 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/libio.h" 1 3 4
# 32 "/usr/include/libio.h" 3 4
# 1 "/usr/include/_G_config.h" 1 3 4
# 15 "/usr/include/_G_config.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h" 1 3 4
# 16 "/usr/include/_G_config.h" 2 3 4




# 1 "/usr/include/wchar.h" 1 3 4
# 82 "/usr/include/wchar.h" 3 4
typedef struct
{
  int __count;
  union
  {

    unsigned int __wch;



    char __wchb[4];
  } __value;
} __mbstate_t;
# 21 "/usr/include/_G_config.h" 2 3 4
typedef struct
{
  __off_t __pos;
  __mbstate_t __state;
} _G_fpos_t;
typedef struct
{
  __off64_t __pos;
  __mbstate_t __state;
} _G_fpos64_t;
# 33 "/usr/include/libio.h" 2 3 4
# 50 "/usr/include/libio.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stdarg.h" 1 3 4
# 40 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 51 "/usr/include/libio.h" 2 3 4
# 145 "/usr/include/libio.h" 3 4
struct _IO_jump_t; struct _IO_FILE;
# 155 "/usr/include/libio.h" 3 4
typedef void _IO_lock_t;





struct _IO_marker {
  struct _IO_marker *_next;
  struct _IO_FILE *_sbuf;



  int _pos;
# 178 "/usr/include/libio.h" 3 4
};


enum __codecvt_result
{
  __codecvt_ok,
  __codecvt_partial,
  __codecvt_error,
  __codecvt_noconv
};
# 246 "/usr/include/libio.h" 3 4
struct _IO_FILE {
  int _flags;




  char* _IO_read_ptr;
  char* _IO_read_end;
  char* _IO_read_base;
  char* _IO_write_base;
  char* _IO_write_ptr;
  char* _IO_write_end;
  char* _IO_buf_base;
  char* _IO_buf_end;

  char *_IO_save_base;
  char *_IO_backup_base;
  char *_IO_save_end;

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;



  int _flags2;

  __off_t _old_offset;



  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];



  _IO_lock_t *_lock;
# 294 "/usr/include/libio.h" 3 4
  __off64_t _offset;
# 303 "/usr/include/libio.h" 3 4
  void *__pad1;
  void *__pad2;
  void *__pad3;
  void *__pad4;
  size_t __pad5;

  int _mode;

  char _unused2[15 * sizeof (int) - 4 * sizeof (void *) - sizeof (size_t)];

};


typedef struct _IO_FILE _IO_FILE;


struct _IO_FILE_plus;

extern struct _IO_FILE_plus _IO_2_1_stdin_;
extern struct _IO_FILE_plus _IO_2_1_stdout_;
extern struct _IO_FILE_plus _IO_2_1_stderr_;
# 339 "/usr/include/libio.h" 3 4
typedef __ssize_t __io_read_fn (void *__cookie, char *__buf, size_t __nbytes);







typedef __ssize_t __io_write_fn (void *__cookie, const char *__buf,
     size_t __n);







typedef int __io_seek_fn (void *__cookie, __off64_t *__pos, int __w);


typedef int __io_close_fn (void *__cookie);
# 391 "/usr/include/libio.h" 3 4
extern int __underflow (_IO_FILE *);
extern int __uflow (_IO_FILE *);
extern int __overflow (_IO_FILE *, int);
# 435 "/usr/include/libio.h" 3 4
extern int _IO_getc (_IO_FILE *__fp);
extern int _IO_putc (int __c, _IO_FILE *__fp);
extern int _IO_feof (_IO_FILE *__fp) __attribute__ ((__nothrow__ , __leaf__));
extern int _IO_ferror (_IO_FILE *__fp) __attribute__ ((__nothrow__ , __leaf__));

extern int _IO_peekc_locked (_IO_FILE *__fp);





extern void _IO_flockfile (_IO_FILE *) __attribute__ ((__nothrow__ , __leaf__));
extern void _IO_funlockfile (_IO_FILE *) __attribute__ ((__nothrow__ , __leaf__));
extern int _IO_ftrylockfile (_IO_FILE *) __attribute__ ((__nothrow__ , __leaf__));
# 465 "/usr/include/libio.h" 3 4
extern int _IO_vfscanf (_IO_FILE * __restrict, const char * __restrict,
   __gnuc_va_list, int *__restrict);
extern int _IO_vfprintf (_IO_FILE *__restrict, const char *__restrict,
    __gnuc_va_list);
extern __ssize_t _IO_padn (_IO_FILE *, int, __ssize_t);
extern size_t _IO_sgetn (_IO_FILE *, void *, size_t);

extern __off64_t _IO_seekoff (_IO_FILE *, __off64_t, int, int);
extern __off64_t _IO_seekpos (_IO_FILE *, __off64_t, int);

extern void _IO_free_backup_area (_IO_FILE *) __attribute__ ((__nothrow__ , __leaf__));
# 75 "/usr/include/stdio.h" 2 3 4




typedef __gnuc_va_list va_list;
# 90 "/usr/include/stdio.h" 3 4
typedef __off_t off_t;
# 102 "/usr/include/stdio.h" 3 4
typedef __ssize_t ssize_t;







typedef _G_fpos_t fpos_t;




# 164 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/stdio_lim.h" 1 3 4
# 165 "/usr/include/stdio.h" 2 3 4



extern struct _IO_FILE *stdin;
extern struct _IO_FILE *stdout;
extern struct _IO_FILE *stderr;







extern int remove (const char *__filename) __attribute__ ((__nothrow__ , __leaf__));

extern int rename (const char *__old, const char *__new) __attribute__ ((__nothrow__ , __leaf__));




extern int renameat (int __oldfd, const char *__old, int __newfd,
       const char *__new) __attribute__ ((__nothrow__ , __leaf__));








extern FILE *tmpfile (void) ;
# 209 "/usr/include/stdio.h" 3 4
extern char *tmpnam (char *__s) __attribute__ ((__nothrow__ , __leaf__)) ;





extern char *tmpnam_r (char *__s) __attribute__ ((__nothrow__ , __leaf__)) ;
# 227 "/usr/include/stdio.h" 3 4
extern char *tempnam (const char *__dir, const char *__pfx)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) ;








extern int fclose (FILE *__stream);




extern int fflush (FILE *__stream);

# 252 "/usr/include/stdio.h" 3 4
extern int fflush_unlocked (FILE *__stream);
# 266 "/usr/include/stdio.h" 3 4






extern FILE *fopen (const char *__restrict __filename,
      const char *__restrict __modes) ;




extern FILE *freopen (const char *__restrict __filename,
        const char *__restrict __modes,
        FILE *__restrict __stream) ;
# 295 "/usr/include/stdio.h" 3 4

# 306 "/usr/include/stdio.h" 3 4
extern FILE *fdopen (int __fd, const char *__modes) __attribute__ ((__nothrow__ , __leaf__)) ;
# 319 "/usr/include/stdio.h" 3 4
extern FILE *fmemopen (void *__s, size_t __len, const char *__modes)
  __attribute__ ((__nothrow__ , __leaf__)) ;




extern FILE *open_memstream (char **__bufloc, size_t *__sizeloc) __attribute__ ((__nothrow__ , __leaf__)) ;






extern void setbuf (FILE *__restrict __stream, char *__restrict __buf) __attribute__ ((__nothrow__ , __leaf__));



extern int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
      int __modes, size_t __n) __attribute__ ((__nothrow__ , __leaf__));





extern void setbuffer (FILE *__restrict __stream, char *__restrict __buf,
         size_t __size) __attribute__ ((__nothrow__ , __leaf__));


extern void setlinebuf (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__));








extern int fprintf (FILE *__restrict __stream,
      const char *__restrict __format, ...);




extern int printf (const char *__restrict __format, ...);

extern int sprintf (char *__restrict __s,
      const char *__restrict __format, ...) __attribute__ ((__nothrow__));





extern int vfprintf (FILE *__restrict __s, const char *__restrict __format,
       __gnuc_va_list __arg);




extern int vprintf (const char *__restrict __format, __gnuc_va_list __arg);

extern int vsprintf (char *__restrict __s, const char *__restrict __format,
       __gnuc_va_list __arg) __attribute__ ((__nothrow__));





extern int snprintf (char *__restrict __s, size_t __maxlen,
       const char *__restrict __format, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 4)));

extern int vsnprintf (char *__restrict __s, size_t __maxlen,
        const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 0)));

# 412 "/usr/include/stdio.h" 3 4
extern int vdprintf (int __fd, const char *__restrict __fmt,
       __gnuc_va_list __arg)
     __attribute__ ((__format__ (__printf__, 2, 0)));
extern int dprintf (int __fd, const char *__restrict __fmt, ...)
     __attribute__ ((__format__ (__printf__, 2, 3)));








extern int fscanf (FILE *__restrict __stream,
     const char *__restrict __format, ...) ;




extern int scanf (const char *__restrict __format, ...) ;

extern int sscanf (const char *__restrict __s,
     const char *__restrict __format, ...) __attribute__ ((__nothrow__ , __leaf__));
# 443 "/usr/include/stdio.h" 3 4
extern int fscanf (FILE *__restrict __stream, const char *__restrict __format, ...) __asm__ ("" "__isoc99_fscanf")

                               ;
extern int scanf (const char *__restrict __format, ...) __asm__ ("" "__isoc99_scanf")
                              ;
extern int sscanf (const char *__restrict __s, const char *__restrict __format, ...) __asm__ ("" "__isoc99_sscanf") __attribute__ ((__nothrow__ , __leaf__))

                      ;
# 463 "/usr/include/stdio.h" 3 4








extern int vfscanf (FILE *__restrict __s, const char *__restrict __format,
      __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 2, 0))) ;





extern int vscanf (const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__format__ (__scanf__, 1, 0))) ;


extern int vsscanf (const char *__restrict __s,
      const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__format__ (__scanf__, 2, 0)));
# 494 "/usr/include/stdio.h" 3 4
extern int vfscanf (FILE *__restrict __s, const char *__restrict __format, __gnuc_va_list __arg) __asm__ ("" "__isoc99_vfscanf")



     __attribute__ ((__format__ (__scanf__, 2, 0))) ;
extern int vscanf (const char *__restrict __format, __gnuc_va_list __arg) __asm__ ("" "__isoc99_vscanf")

     __attribute__ ((__format__ (__scanf__, 1, 0))) ;
extern int vsscanf (const char *__restrict __s, const char *__restrict __format, __gnuc_va_list __arg) __asm__ ("" "__isoc99_vsscanf") __attribute__ ((__nothrow__ , __leaf__))



     __attribute__ ((__format__ (__scanf__, 2, 0)));
# 522 "/usr/include/stdio.h" 3 4









extern int fgetc (FILE *__stream);
extern int getc (FILE *__stream);





extern int getchar (void);

# 550 "/usr/include/stdio.h" 3 4
extern int getc_unlocked (FILE *__stream);
extern int getchar_unlocked (void);
# 561 "/usr/include/stdio.h" 3 4
extern int fgetc_unlocked (FILE *__stream);











extern int fputc (int __c, FILE *__stream);
extern int putc (int __c, FILE *__stream);





extern int putchar (int __c);

# 594 "/usr/include/stdio.h" 3 4
extern int fputc_unlocked (int __c, FILE *__stream);







extern int putc_unlocked (int __c, FILE *__stream);
extern int putchar_unlocked (int __c);






extern int getw (FILE *__stream);


extern int putw (int __w, FILE *__stream);








extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream)
     ;
# 638 "/usr/include/stdio.h" 3 4
extern char *gets (char *__s) __attribute__ ((__deprecated__));


# 665 "/usr/include/stdio.h" 3 4
extern __ssize_t __getdelim (char **__restrict __lineptr,
          size_t *__restrict __n, int __delimiter,
          FILE *__restrict __stream) ;
extern __ssize_t getdelim (char **__restrict __lineptr,
        size_t *__restrict __n, int __delimiter,
        FILE *__restrict __stream) ;







extern __ssize_t getline (char **__restrict __lineptr,
       size_t *__restrict __n,
       FILE *__restrict __stream) ;








extern int fputs (const char *__restrict __s, FILE *__restrict __stream);





extern int puts (const char *__s);






extern int ungetc (int __c, FILE *__stream);






extern size_t fread (void *__restrict __ptr, size_t __size,
       size_t __n, FILE *__restrict __stream) ;




extern size_t fwrite (const void *__restrict __ptr, size_t __size,
        size_t __n, FILE *__restrict __s);

# 737 "/usr/include/stdio.h" 3 4
extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
         size_t __n, FILE *__restrict __stream) ;
extern size_t fwrite_unlocked (const void *__restrict __ptr, size_t __size,
          size_t __n, FILE *__restrict __stream);








extern int fseek (FILE *__stream, long int __off, int __whence);




extern long int ftell (FILE *__stream) ;




extern void rewind (FILE *__stream);

# 773 "/usr/include/stdio.h" 3 4
extern int fseeko (FILE *__stream, __off_t __off, int __whence);




extern __off_t ftello (FILE *__stream) ;
# 792 "/usr/include/stdio.h" 3 4






extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos);




extern int fsetpos (FILE *__stream, const fpos_t *__pos);
# 815 "/usr/include/stdio.h" 3 4

# 824 "/usr/include/stdio.h" 3 4


extern void clearerr (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__));

extern int feof (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;

extern int ferror (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;




extern void clearerr_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__));
extern int feof_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;
extern int ferror_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;








extern void perror (const char *__s);






# 1 "/usr/include/x86_64-linux-gnu/bits/sys_errlist.h" 1 3 4
# 26 "/usr/include/x86_64-linux-gnu/bits/sys_errlist.h" 3 4
extern int sys_nerr;
extern const char *const sys_errlist[];
# 854 "/usr/include/stdio.h" 2 3 4




extern int fileno (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;




extern int fileno_unlocked (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;
# 873 "/usr/include/stdio.h" 3 4
extern FILE *popen (const char *__command, const char *__modes) ;





extern int pclose (FILE *__stream);





extern char *ctermid (char *__s) __attribute__ ((__nothrow__ , __leaf__));
# 913 "/usr/include/stdio.h" 3 4
extern void flockfile (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__));



extern int ftrylockfile (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__)) ;


extern void funlockfile (FILE *__stream) __attribute__ ((__nothrow__ , __leaf__));
# 943 "/usr/include/stdio.h" 3 4

# 7 "../../include/basics.h" 2
# 1 "/usr/include/stdlib.h" 1 3 4
# 32 "/usr/include/stdlib.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h" 1 3 4
# 324 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h" 3 4
typedef int wchar_t;
# 33 "/usr/include/stdlib.h" 2 3 4








# 1 "/usr/include/x86_64-linux-gnu/bits/waitflags.h" 1 3 4
# 42 "/usr/include/stdlib.h" 2 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/waitstatus.h" 1 3 4
# 64 "/usr/include/x86_64-linux-gnu/bits/waitstatus.h" 3 4
# 1 "/usr/include/endian.h" 1 3 4
# 36 "/usr/include/endian.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/endian.h" 1 3 4
# 37 "/usr/include/endian.h" 2 3 4
# 60 "/usr/include/endian.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/byteswap.h" 1 3 4
# 28 "/usr/include/x86_64-linux-gnu/bits/byteswap.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 29 "/usr/include/x86_64-linux-gnu/bits/byteswap.h" 2 3 4






# 1 "/usr/include/x86_64-linux-gnu/bits/byteswap-16.h" 1 3 4
# 36 "/usr/include/x86_64-linux-gnu/bits/byteswap.h" 2 3 4
# 44 "/usr/include/x86_64-linux-gnu/bits/byteswap.h" 3 4
static __inline unsigned int
__bswap_32 (unsigned int __bsx)
{
  return __builtin_bswap32 (__bsx);
}
# 108 "/usr/include/x86_64-linux-gnu/bits/byteswap.h" 3 4
static __inline __uint64_t
__bswap_64 (__uint64_t __bsx)
{
  return __builtin_bswap64 (__bsx);
}
# 61 "/usr/include/endian.h" 2 3 4
# 65 "/usr/include/x86_64-linux-gnu/bits/waitstatus.h" 2 3 4

union wait
  {
    int w_status;
    struct
      {

 unsigned int __w_termsig:7;
 unsigned int __w_coredump:1;
 unsigned int __w_retcode:8;
 unsigned int:16;







      } __wait_terminated;
    struct
      {

 unsigned int __w_stopval:8;
 unsigned int __w_stopsig:8;
 unsigned int:16;






      } __wait_stopped;
  };
# 43 "/usr/include/stdlib.h" 2 3 4
# 67 "/usr/include/stdlib.h" 3 4
typedef union
  {
    union wait *__uptr;
    int *__iptr;
  } __WAIT_STATUS __attribute__ ((__transparent_union__));
# 95 "/usr/include/stdlib.h" 3 4


typedef struct
  {
    int quot;
    int rem;
  } div_t;



typedef struct
  {
    long int quot;
    long int rem;
  } ldiv_t;







__extension__ typedef struct
  {
    long long int quot;
    long long int rem;
  } lldiv_t;


# 139 "/usr/include/stdlib.h" 3 4
extern size_t __ctype_get_mb_cur_max (void) __attribute__ ((__nothrow__ , __leaf__)) ;




extern double atof (const char *__nptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;

extern int atoi (const char *__nptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;

extern long int atol (const char *__nptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;





__extension__ extern long long int atoll (const char *__nptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;





extern double strtod (const char *__restrict __nptr,
        char **__restrict __endptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));





extern float strtof (const char *__restrict __nptr,
       char **__restrict __endptr) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

extern long double strtold (const char *__restrict __nptr,
       char **__restrict __endptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));





extern long int strtol (const char *__restrict __nptr,
   char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

extern unsigned long int strtoul (const char *__restrict __nptr,
      char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));




__extension__
extern long long int strtoq (const char *__restrict __nptr,
        char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

__extension__
extern unsigned long long int strtouq (const char *__restrict __nptr,
           char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));





__extension__
extern long long int strtoll (const char *__restrict __nptr,
         char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

__extension__
extern unsigned long long int strtoull (const char *__restrict __nptr,
     char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));

# 305 "/usr/include/stdlib.h" 3 4
extern char *l64a (long int __n) __attribute__ ((__nothrow__ , __leaf__)) ;


extern long int a64l (const char *__s)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1))) ;




# 1 "/usr/include/x86_64-linux-gnu/sys/types.h" 1 3 4
# 27 "/usr/include/x86_64-linux-gnu/sys/types.h" 3 4






typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;




typedef __loff_t loff_t;



typedef __ino_t ino_t;
# 60 "/usr/include/x86_64-linux-gnu/sys/types.h" 3 4
typedef __dev_t dev_t;




typedef __gid_t gid_t;




typedef __mode_t mode_t;




typedef __nlink_t nlink_t;




typedef __uid_t uid_t;
# 98 "/usr/include/x86_64-linux-gnu/sys/types.h" 3 4
typedef __pid_t pid_t;





typedef __id_t id_t;
# 115 "/usr/include/x86_64-linux-gnu/sys/types.h" 3 4
typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;





typedef __key_t key_t;
# 132 "/usr/include/x86_64-linux-gnu/sys/types.h" 3 4
# 1 "/usr/include/time.h" 1 3 4
# 57 "/usr/include/time.h" 3 4


typedef __clock_t clock_t;



# 73 "/usr/include/time.h" 3 4


typedef __time_t time_t;



# 91 "/usr/include/time.h" 3 4
typedef __clockid_t clockid_t;
# 103 "/usr/include/time.h" 3 4
typedef __timer_t timer_t;
# 133 "/usr/include/x86_64-linux-gnu/sys/types.h" 2 3 4
# 146 "/usr/include/x86_64-linux-gnu/sys/types.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h" 1 3 4
# 147 "/usr/include/x86_64-linux-gnu/sys/types.h" 2 3 4



typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;
# 194 "/usr/include/x86_64-linux-gnu/sys/types.h" 3 4
typedef int int8_t __attribute__ ((__mode__ (__QI__)));
typedef int int16_t __attribute__ ((__mode__ (__HI__)));
typedef int int32_t __attribute__ ((__mode__ (__SI__)));
typedef int int64_t __attribute__ ((__mode__ (__DI__)));


typedef unsigned int u_int8_t __attribute__ ((__mode__ (__QI__)));
typedef unsigned int u_int16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int u_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int u_int64_t __attribute__ ((__mode__ (__DI__)));

typedef int register_t __attribute__ ((__mode__ (__word__)));
# 219 "/usr/include/x86_64-linux-gnu/sys/types.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/sys/select.h" 1 3 4
# 30 "/usr/include/x86_64-linux-gnu/sys/select.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/select.h" 1 3 4
# 22 "/usr/include/x86_64-linux-gnu/bits/select.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 23 "/usr/include/x86_64-linux-gnu/bits/select.h" 2 3 4
# 31 "/usr/include/x86_64-linux-gnu/sys/select.h" 2 3 4


# 1 "/usr/include/x86_64-linux-gnu/bits/sigset.h" 1 3 4
# 23 "/usr/include/x86_64-linux-gnu/bits/sigset.h" 3 4
typedef int __sig_atomic_t;




typedef struct
  {
    unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
  } __sigset_t;
# 34 "/usr/include/x86_64-linux-gnu/sys/select.h" 2 3 4



typedef __sigset_t sigset_t;





# 1 "/usr/include/time.h" 1 3 4
# 120 "/usr/include/time.h" 3 4
struct timespec
  {
    __time_t tv_sec;
    __syscall_slong_t tv_nsec;
  };
# 44 "/usr/include/x86_64-linux-gnu/sys/select.h" 2 3 4

# 1 "/usr/include/x86_64-linux-gnu/bits/time.h" 1 3 4
# 30 "/usr/include/x86_64-linux-gnu/bits/time.h" 3 4
struct timeval
  {
    __time_t tv_sec;
    __suseconds_t tv_usec;
  };
# 46 "/usr/include/x86_64-linux-gnu/sys/select.h" 2 3 4


typedef __suseconds_t suseconds_t;





typedef long int __fd_mask;
# 64 "/usr/include/x86_64-linux-gnu/sys/select.h" 3 4
typedef struct
  {






    __fd_mask __fds_bits[1024 / (8 * (int) sizeof (__fd_mask))];


  } fd_set;






typedef __fd_mask fd_mask;
# 96 "/usr/include/x86_64-linux-gnu/sys/select.h" 3 4

# 106 "/usr/include/x86_64-linux-gnu/sys/select.h" 3 4
extern int select (int __nfds, fd_set *__restrict __readfds,
     fd_set *__restrict __writefds,
     fd_set *__restrict __exceptfds,
     struct timeval *__restrict __timeout);
# 118 "/usr/include/x86_64-linux-gnu/sys/select.h" 3 4
extern int pselect (int __nfds, fd_set *__restrict __readfds,
      fd_set *__restrict __writefds,
      fd_set *__restrict __exceptfds,
      const struct timespec *__restrict __timeout,
      const __sigset_t *__restrict __sigmask);
# 131 "/usr/include/x86_64-linux-gnu/sys/select.h" 3 4

# 220 "/usr/include/x86_64-linux-gnu/sys/types.h" 2 3 4


# 1 "/usr/include/x86_64-linux-gnu/sys/sysmacros.h" 1 3 4
# 29 "/usr/include/x86_64-linux-gnu/sys/sysmacros.h" 3 4


__extension__
extern unsigned int gnu_dev_major (unsigned long long int __dev)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));
__extension__
extern unsigned int gnu_dev_minor (unsigned long long int __dev)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));
__extension__
extern unsigned long long int gnu_dev_makedev (unsigned int __major,
            unsigned int __minor)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));
# 63 "/usr/include/x86_64-linux-gnu/sys/sysmacros.h" 3 4

# 223 "/usr/include/x86_64-linux-gnu/sys/types.h" 2 3 4





typedef __blksize_t blksize_t;






typedef __blkcnt_t blkcnt_t;



typedef __fsblkcnt_t fsblkcnt_t;



typedef __fsfilcnt_t fsfilcnt_t;
# 270 "/usr/include/x86_64-linux-gnu/sys/types.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h" 1 3 4
# 21 "/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/wordsize.h" 1 3 4
# 22 "/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h" 2 3 4
# 60 "/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h" 3 4
typedef unsigned long int pthread_t;


union pthread_attr_t
{
  char __size[56];
  long int __align;
};

typedef union pthread_attr_t pthread_attr_t;





typedef struct __pthread_internal_list
{
  struct __pthread_internal_list *__prev;
  struct __pthread_internal_list *__next;
} __pthread_list_t;
# 90 "/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h" 3 4
typedef union
{
  struct __pthread_mutex_s
  {
    int __lock;
    unsigned int __count;
    int __owner;

    unsigned int __nusers;



    int __kind;

    int __spins;
    __pthread_list_t __list;
# 115 "/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h" 3 4
  } __data;
  char __size[40];
  long int __align;
} pthread_mutex_t;

typedef union
{
  char __size[4];
  int __align;
} pthread_mutexattr_t;




typedef union
{
  struct
  {
    int __lock;
    unsigned int __futex;
    __extension__ unsigned long long int __total_seq;
    __extension__ unsigned long long int __wakeup_seq;
    __extension__ unsigned long long int __woken_seq;
    void *__mutex;
    unsigned int __nwaiters;
    unsigned int __broadcast_seq;
  } __data;
  char __size[48];
  __extension__ long long int __align;
} pthread_cond_t;

typedef union
{
  char __size[4];
  int __align;
} pthread_condattr_t;



typedef unsigned int pthread_key_t;



typedef int pthread_once_t;





typedef union
{

  struct
  {
    int __lock;
    unsigned int __nr_readers;
    unsigned int __readers_wakeup;
    unsigned int __writer_wakeup;
    unsigned int __nr_readers_queued;
    unsigned int __nr_writers_queued;
    int __writer;
    int __shared;
    unsigned long int __pad1;
    unsigned long int __pad2;


    unsigned int __flags;

  } __data;
# 202 "/usr/include/x86_64-linux-gnu/bits/pthreadtypes.h" 3 4
  char __size[56];
  long int __align;
} pthread_rwlock_t;

typedef union
{
  char __size[8];
  long int __align;
} pthread_rwlockattr_t;





typedef volatile int pthread_spinlock_t;




typedef union
{
  char __size[32];
  long int __align;
} pthread_barrier_t;

typedef union
{
  char __size[4];
  int __align;
} pthread_barrierattr_t;
# 271 "/usr/include/x86_64-linux-gnu/sys/types.h" 2 3 4



# 315 "/usr/include/stdlib.h" 2 3 4






extern long int random (void) __attribute__ ((__nothrow__ , __leaf__));


extern void srandom (unsigned int __seed) __attribute__ ((__nothrow__ , __leaf__));





extern char *initstate (unsigned int __seed, char *__statebuf,
   size_t __statelen) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));



extern char *setstate (char *__statebuf) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));







struct random_data
  {
    int32_t *fptr;
    int32_t *rptr;
    int32_t *state;
    int rand_type;
    int rand_deg;
    int rand_sep;
    int32_t *end_ptr;
  };

extern int random_r (struct random_data *__restrict __buf,
       int32_t *__restrict __result) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));

extern int srandom_r (unsigned int __seed, struct random_data *__buf)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));

extern int initstate_r (unsigned int __seed, char *__restrict __statebuf,
   size_t __statelen,
   struct random_data *__restrict __buf)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 4)));

extern int setstate_r (char *__restrict __statebuf,
         struct random_data *__restrict __buf)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));






extern int rand (void) __attribute__ ((__nothrow__ , __leaf__));

extern void srand (unsigned int __seed) __attribute__ ((__nothrow__ , __leaf__));




extern int rand_r (unsigned int *__seed) __attribute__ ((__nothrow__ , __leaf__));







extern double drand48 (void) __attribute__ ((__nothrow__ , __leaf__));
extern double erand48 (unsigned short int __xsubi[3]) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern long int lrand48 (void) __attribute__ ((__nothrow__ , __leaf__));
extern long int nrand48 (unsigned short int __xsubi[3])
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern long int mrand48 (void) __attribute__ ((__nothrow__ , __leaf__));
extern long int jrand48 (unsigned short int __xsubi[3])
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern void srand48 (long int __seedval) __attribute__ ((__nothrow__ , __leaf__));
extern unsigned short int *seed48 (unsigned short int __seed16v[3])
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
extern void lcong48 (unsigned short int __param[7]) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));





struct drand48_data
  {
    unsigned short int __x[3];
    unsigned short int __old_x[3];
    unsigned short int __c;
    unsigned short int __init;
    unsigned long long int __a;
  };


extern int drand48_r (struct drand48_data *__restrict __buffer,
        double *__restrict __result) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern int erand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        double *__restrict __result) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern int lrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern int nrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern int mrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern int jrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern int srand48_r (long int __seedval, struct drand48_data *__buffer)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));

extern int seed48_r (unsigned short int __seed16v[3],
       struct drand48_data *__buffer) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));

extern int lcong48_r (unsigned short int __param[7],
        struct drand48_data *__buffer)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));









extern void *malloc (size_t __size) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) ;

extern void *calloc (size_t __nmemb, size_t __size)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) ;










extern void *realloc (void *__ptr, size_t __size)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__warn_unused_result__));

extern void free (void *__ptr) __attribute__ ((__nothrow__ , __leaf__));




extern void cfree (void *__ptr) __attribute__ ((__nothrow__ , __leaf__));



# 1 "/usr/include/alloca.h" 1 3 4
# 24 "/usr/include/alloca.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h" 1 3 4
# 25 "/usr/include/alloca.h" 2 3 4







extern void *alloca (size_t __size) __attribute__ ((__nothrow__ , __leaf__));






# 492 "/usr/include/stdlib.h" 2 3 4





extern void *valloc (size_t __size) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) ;




extern int posix_memalign (void **__memptr, size_t __alignment, size_t __size)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1))) ;
# 512 "/usr/include/stdlib.h" 3 4


extern void abort (void) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));



extern int atexit (void (*__func) (void)) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
# 529 "/usr/include/stdlib.h" 3 4





extern int on_exit (void (*__func) (int __status, void *__arg), void *__arg)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));






extern void exit (int __status) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));













extern void _Exit (int __status) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));






extern char *getenv (const char *__name) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1))) ;

# 577 "/usr/include/stdlib.h" 3 4
extern int putenv (char *__string) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));





extern int setenv (const char *__name, const char *__value, int __replace)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));


extern int unsetenv (const char *__name) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));






extern int clearenv (void) __attribute__ ((__nothrow__ , __leaf__));
# 605 "/usr/include/stdlib.h" 3 4
extern char *mktemp (char *__template) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));
# 619 "/usr/include/stdlib.h" 3 4
extern int mkstemp (char *__template) __attribute__ ((__nonnull__ (1))) ;
# 641 "/usr/include/stdlib.h" 3 4
extern int mkstemps (char *__template, int __suffixlen) __attribute__ ((__nonnull__ (1))) ;
# 662 "/usr/include/stdlib.h" 3 4
extern char *mkdtemp (char *__template) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1))) ;
# 711 "/usr/include/stdlib.h" 3 4





extern int system (const char *__command) ;

# 733 "/usr/include/stdlib.h" 3 4
extern char *realpath (const char *__restrict __name,
         char *__restrict __resolved) __attribute__ ((__nothrow__ , __leaf__)) ;






typedef int (*__compar_fn_t) (const void *, const void *);
# 751 "/usr/include/stdlib.h" 3 4



extern void *bsearch (const void *__key, const void *__base,
        size_t __nmemb, size_t __size, __compar_fn_t __compar)
     __attribute__ ((__nonnull__ (1, 2, 5))) ;



extern void qsort (void *__base, size_t __nmemb, size_t __size,
     __compar_fn_t __compar) __attribute__ ((__nonnull__ (1, 4)));
# 770 "/usr/include/stdlib.h" 3 4
extern int abs (int __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;
extern long int labs (long int __x) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;



__extension__ extern long long int llabs (long long int __x)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;







extern div_t div (int __numer, int __denom)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;
extern ldiv_t ldiv (long int __numer, long int __denom)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;




__extension__ extern lldiv_t lldiv (long long int __numer,
        long long int __denom)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__)) ;

# 807 "/usr/include/stdlib.h" 3 4
extern char *ecvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4))) ;




extern char *fcvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4))) ;




extern char *gcvt (double __value, int __ndigit, char *__buf)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3))) ;




extern char *qecvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4))) ;
extern char *qfcvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4))) ;
extern char *qgcvt (long double __value, int __ndigit, char *__buf)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3))) ;




extern int ecvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4, 5)));
extern int fcvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4, 5)));

extern int qecvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4, 5)));
extern int qfcvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (3, 4, 5)));







extern int mblen (const char *__s, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) ;


extern int mbtowc (wchar_t *__restrict __pwc,
     const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) ;


extern int wctomb (char *__s, wchar_t __wchar) __attribute__ ((__nothrow__ , __leaf__)) ;



extern size_t mbstowcs (wchar_t *__restrict __pwcs,
   const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__ , __leaf__));

extern size_t wcstombs (char *__restrict __s,
   const wchar_t *__restrict __pwcs, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__));








extern int rpmatch (const char *__response) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1))) ;
# 895 "/usr/include/stdlib.h" 3 4
extern int getsubopt (char **__restrict __optionp,
        char *const *__restrict __tokens,
        char **__restrict __valuep)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2, 3))) ;
# 947 "/usr/include/stdlib.h" 3 4
extern int getloadavg (double __loadavg[], int __nelem)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


# 1 "/usr/include/x86_64-linux-gnu/bits/stdlib-float.h" 1 3 4
# 952 "/usr/include/stdlib.h" 2 3 4
# 964 "/usr/include/stdlib.h" 3 4

# 8 "../../include/basics.h" 2
# 1 "/usr/include/string.h" 1 3 4
# 27 "/usr/include/string.h" 3 4





# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h" 1 3 4
# 33 "/usr/include/string.h" 2 3 4









extern void *memcpy (void *__restrict __dest, const void *__restrict __src,
       size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern void *memmove (void *__dest, const void *__src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));






extern void *memccpy (void *__restrict __dest, const void *__restrict __src,
        int __c, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));





extern void *memset (void *__s, int __c, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern int memcmp (const void *__s1, const void *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 92 "/usr/include/string.h" 3 4
extern void *memchr (const void *__s, int __c, size_t __n)
      __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));


# 123 "/usr/include/string.h" 3 4


extern char *strcpy (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));

extern char *strncpy (char *__restrict __dest,
        const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern char *strcat (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));

extern char *strncat (char *__restrict __dest, const char *__restrict __src,
        size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern int strcmp (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));

extern int strncmp (const char *__s1, const char *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern int strcoll (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));

extern size_t strxfrm (char *__restrict __dest,
         const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));






# 1 "/usr/include/xlocale.h" 1 3 4
# 27 "/usr/include/xlocale.h" 3 4
typedef struct __locale_struct
{

  struct __locale_data *__locales[13];


  const unsigned short int *__ctype_b;
  const int *__ctype_tolower;
  const int *__ctype_toupper;


  const char *__names[13];
} *__locale_t;


typedef __locale_t locale_t;
# 160 "/usr/include/string.h" 2 3 4


extern int strcoll_l (const char *__s1, const char *__s2, __locale_t __l)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2, 3)));

extern size_t strxfrm_l (char *__dest, const char *__src, size_t __n,
    __locale_t __l) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 4)));





extern char *strdup (const char *__s)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) __attribute__ ((__nonnull__ (1)));






extern char *strndup (const char *__string, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__malloc__)) __attribute__ ((__nonnull__ (1)));
# 207 "/usr/include/string.h" 3 4

# 232 "/usr/include/string.h" 3 4
extern char *strchr (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
# 259 "/usr/include/string.h" 3 4
extern char *strrchr (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));


# 278 "/usr/include/string.h" 3 4



extern size_t strcspn (const char *__s, const char *__reject)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern size_t strspn (const char *__s, const char *__accept)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 311 "/usr/include/string.h" 3 4
extern char *strpbrk (const char *__s, const char *__accept)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 338 "/usr/include/string.h" 3 4
extern char *strstr (const char *__haystack, const char *__needle)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));




extern char *strtok (char *__restrict __s, const char *__restrict __delim)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2)));




extern char *__strtok_r (char *__restrict __s,
    const char *__restrict __delim,
    char **__restrict __save_ptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 3)));

extern char *strtok_r (char *__restrict __s, const char *__restrict __delim,
         char **__restrict __save_ptr)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (2, 3)));
# 393 "/usr/include/string.h" 3 4


extern size_t strlen (const char *__s)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));





extern size_t strnlen (const char *__string, size_t __maxlen)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));





extern char *strerror (int __errnum) __attribute__ ((__nothrow__ , __leaf__));

# 423 "/usr/include/string.h" 3 4
extern int strerror_r (int __errnum, char *__buf, size_t __buflen) __asm__ ("" "__xpg_strerror_r") __attribute__ ((__nothrow__ , __leaf__))

                        __attribute__ ((__nonnull__ (2)));
# 441 "/usr/include/string.h" 3 4
extern char *strerror_l (int __errnum, __locale_t __l) __attribute__ ((__nothrow__ , __leaf__));





extern void __bzero (void *__s, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));



extern void bcopy (const void *__src, void *__dest, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));


extern void bzero (void *__s, size_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1)));


extern int bcmp (const void *__s1, const void *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 485 "/usr/include/string.h" 3 4
extern char *index (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
# 513 "/usr/include/string.h" 3 4
extern char *rindex (const char *__s, int __c)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));




extern int ffs (int __i) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));
# 532 "/usr/include/string.h" 3 4
extern int strcasecmp (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));


extern int strncasecmp (const char *__s1, const char *__s2, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));
# 555 "/usr/include/string.h" 3 4
extern char *strsep (char **__restrict __stringp,
       const char *__restrict __delim)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));




extern char *strsignal (int __sig) __attribute__ ((__nothrow__ , __leaf__));


extern char *__stpcpy (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *stpcpy (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));



extern char *__stpncpy (char *__restrict __dest,
   const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
extern char *stpncpy (char *__restrict __dest,
        const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__nonnull__ (1, 2)));
# 642 "/usr/include/string.h" 3 4

# 9 "../../include/basics.h" 2

# 1 "../../include/config.h" 1
       

# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed/limits.h" 1 3 4
# 34 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed/limits.h" 3 4
# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed/syslimits.h" 1 3 4






# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed/limits.h" 1 3 4
# 168 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed/limits.h" 3 4
# 1 "/usr/include/limits.h" 1 3 4
# 144 "/usr/include/limits.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/posix1_lim.h" 1 3 4
# 160 "/usr/include/x86_64-linux-gnu/bits/posix1_lim.h" 3 4
# 1 "/usr/include/x86_64-linux-gnu/bits/local_lim.h" 1 3 4
# 38 "/usr/include/x86_64-linux-gnu/bits/local_lim.h" 3 4
# 1 "/usr/include/linux/limits.h" 1 3 4
# 39 "/usr/include/x86_64-linux-gnu/bits/local_lim.h" 2 3 4
# 161 "/usr/include/x86_64-linux-gnu/bits/posix1_lim.h" 2 3 4
# 145 "/usr/include/limits.h" 2 3 4



# 1 "/usr/include/x86_64-linux-gnu/bits/posix2_lim.h" 1 3 4
# 149 "/usr/include/limits.h" 2 3 4
# 169 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed/limits.h" 2 3 4
# 8 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed/syslimits.h" 2 3 4
# 35 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed/limits.h" 2 3 4
# 4 "../../include/config.h" 2
# 14 "../../include/config.h"
typedef char TARGET_CHAR;
typedef signed char TARGET_SCHAR;
typedef unsigned char TARGET_UCHAR;
typedef signed short TARGET_SSHORT;
typedef unsigned short TARGET_USHORT;
typedef signed int TARGET_SINT;
typedef unsigned int TARGET_UINT;
typedef signed long TARGET_SLONG;
typedef unsigned long TARGET_ULONG;
typedef signed long long TARGET_SLLONG;
typedef unsigned long long TARGET_ULLONG;
# 54 "../../include/config.h"
# 1 "../../include/arch/x86_64.h" 1
# 55 "../../include/config.h" 2
# 11 "../../include/basics.h" 2


# 29 "../../include/basics.h"
typedef void Generic;
typedef void **GenericREF;


typedef unsigned char bool;

typedef bool Bool;
# 56 "../../include/basics.h"
typedef union nodeStruct Node;
typedef struct tablestruct SymbolTable;
typedef int OpType;

typedef struct coord
{
  int line;
  short offset;
  short file;
} Coord;

 extern Coord const UnknownCoord;
 extern Coord const DontPrintCoord;
 Bool IsUnknownCoord (Coord coord);
 Bool IsDontPrintCoord (Coord coord);
# 81 "../../include/basics.h"
 void minf_coord (Coord *x, Coord const *y);



# 1 "../../include/gbuf.h" 1
       

# 1 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stdarg.h" 1 3 4
# 4 "../../include/gbuf.h" 2




typedef struct GBUF GBUF;

 extern int gbprintf (GBUF *gb, char const *format, ...) __attribute__ ((format (printf, 2, 3)));
 int gbputs (char const *string, GBUF *gb);

 GBUF *make_string_gbuf (char *array, int arraysize);
 GBUF *make_file_gbuf (FILE *f);
 void free_gbuf (GBUF *);




# 86 "../../include/basics.h" 2
# 1 "../../include/heap.h" 1
       
# 87 "../../include/basics.h" 2
# 1 "../../include/list.h" 1
       



typedef struct liststruct List;

typedef struct
{
  List *first;
  List *current;
  List *tail;
} ListMarker;

 Generic *FirstItem (List *list);
 Generic *LastItem (List *list);
 Generic *SetItem (List *list, Generic *element);
 List *Rest (List *list);
 List *Last (List *list);
 int ListLength (List *list);

 List *FindItem (List *list, Generic *item);
 List *RemoveItem (List *list, Generic *item);

 List *MakeNewList (Generic *item);
 List *ConsItem (Generic *item, List *list);
 List *AppendItem (List *list, Generic *item);
 List *JoinLists (List *list1, List *list2);
 List *ListCopy (List *list);


 List *List2 (Generic *x1, Generic *x2);
 List *List3 (Generic *x1, Generic *x2, Generic *x3);
 List *List4 (Generic *x1, Generic *x2, Generic *x3, Generic *x4);
 List *List5 (Generic *x1, Generic *x2, Generic *x3, Generic *x4, Generic *x5);


 void IterateList (ListMarker *, List *);
 Bool NextOnList (ListMarker *, GenericREF itemref);
 List *InsertList (ListMarker *marker, List *list);
 List *SplitList (ListMarker *marker);
 void SetCurrentOnList (ListMarker *marker, Generic *handle);
 List *NextChunkOnList (ListMarker *, int chunksize);
 Bool EndOfList (ListMarker *marker);


 List *Mapc (List *, void f (Generic *));

 List *DeleteIf (List *, Bool f (Generic *));

 void
DualListWalk ( List *l1, List *l2, void *answer
             , void code (Node *n1, Node *n2, void *answer)
             , Bool continue_code (Node *n1, Node *n2, void *answer)
             , void fail_code (void *answer)
             );


# 88 "../../include/basics.h" 2
# 1 "../../include/symbol.h" 1
       




typedef enum
{
  Nested, Flat
} TableType;

 extern short Level;

typedef void ConflictProc (Generic *orig, Generic *create);
typedef void ShadowProc (Generic *create, Generic *shadowed);
typedef void ExitscopeProc (Generic *dead);






 SymbolTable *NewSymbolTable (char const *name, TableType kind,
                                    ShadowProc, ExitscopeProc);

 void ResetSymbolTable (SymbolTable *table);

 void PrintSymbolTable (FILE *out, SymbolTable *table);

 void EnterScope (void);
 void ExitScope (void);

 Bool LookupSymbol (SymbolTable *, char const *name, Generic **var);

 Generic *InsertSymbol (SymbolTable *, char const *name, Generic * var,
                              ConflictProc);

 void MoveToOuterScope (SymbolTable *, char const *name);

 char const *InsertUniqueSymbol (SymbolTable *table, Generic *var, char const *root);

typedef struct
{
  SymbolTable *table;
  int i;
  void *chain;
} SymbolTableMarker;

 void IterateSymbolTable (SymbolTableMarker *, SymbolTable *);
 Bool NextInSymbolTable (SymbolTableMarker *, char const **name, GenericREF itemref);


# 89 "../../include/basics.h" 2



 extern char const *Executable;
 extern List *Program;
 extern int WarningLevel;
 extern int Line, LineOffset, Errors, Warnings;
 extern unsigned short CurrentFile;
 extern char *Filename;
 extern char *FileNames[1024];
 extern char const *PhaseName;
 extern Bool GenerateSegments;


 extern Bool MemoryChecking;
 extern Bool OptimiseChecking;
 extern Bool MemoryCheckedp;
 extern Bool MemoryWarningp;


 extern SymbolTable *Identifiers, *Labels, *Tags;


 extern SymbolTable *Externals;


 extern Bool DebugLex;
 extern Bool PrintLineOffset;
 extern Bool IgnoreLineDirectives;
 extern Bool ANSIOnly;
 extern Bool FormatReadably;
 extern Bool PrintLiveVars;
 extern Bool PrintASTcoords;




 void DPN (Node *n);
 void DPL (List *list);
 void PrintNode (GBUF *out, Node *node, int tab_depth);

 int print_float (GBUF *, float);
 int print_double (GBUF *, double);
 void PrintCRSpaces (GBUF *out, int spaces);
 void PrintSpaces (GBUF *out, int spaces);
 void PrintList (GBUF *out, List *list, int tab_depth);

 int PrintOp (GBUF *, OpType op);
 void CharToText (char *array, unsigned char value);
 int PrintChar (GBUF *out, int c);
 int PrintString (GBUF *out, char const *string);


 extern void Fail (char const *file, int line, char const *msg) __attribute__ ((noreturn));
 void SyntaxError (char const *fmt, ...);
 void SyntaxErrorCoord (Coord c, char const *fmt, ...);
 void Warning (int level, char const *fmt, ...);
 void WarningCoord (int level, Coord c, char const *fmt, ...);


 void SwitchLexInputToString (char const *str);
 void DeleteLexInputBuffer (void);
 Bool OmitLookups (void);
 char *UniqueString (char const *string);
 void SetFile (char const *filename, int line);


 List *SemanticCheckProgram (List *program);
 Node *SemCheckNode (Node *node);
 List *SemCheckList (List *list);


 Bool SideeffectsNode (Node *);


 void OutputProgram (FILE *out, List *program);

 void OutputStmtList (FILE *outfile, List *myStmtList);

 void output_constant_to_GBUF (GBUF *, Node *, Bool );
 void output_prim_type_to_GBUF (GBUF *gb, Node *type, char *lastc);


# 4 "../../include/ast.h" 2
# 1 "../../include/dataflow.h" 1
       





typedef unsigned long BitVector;





typedef struct FlowValue
{
  Bool undefined;
  BitVector bitvector;
} FlowValue;

typedef struct Analysis
{

  FlowValue gen;
  FlowValue kill;


  List *livevars;
  List *dirtyvars;
} Analysis;

typedef enum Direction
{
  Backwards,
  Forwards
} Direction;

typedef enum Point
{
  EntryPoint,
  ExitPoint
} Point;

typedef FlowValue MeetOp (FlowValue, FlowValue);
typedef FlowValue TransOp (Node *, FlowValue, Point, Bool);
typedef Bool EqualOp (FlowValue, FlowValue);


 void IterateDataFlow (Node *root,
                             FlowValue init,
                             Direction dir,
                             MeetOp meet,
                             EqualOp equal,
                             TransOp trans
                             );


 List *RegisterVariables (Node *node, List *vars);

 void PV (List *vars);
 int PrintVariables (GBUF *, List *vars);
 int PrintAnalysis (GBUF *, Node *node);

 void AnalyseLiveVariables (Node *root, List *vars);
 void AnalyseReturnFlow (Node *root);

 void AnalyseProgram (List *program);


# 5 "../../include/ast.h" 2
# 1 "../../include/typequal.h" 1
       









typedef struct typequal
{
  unsigned int tq;
  List *attributes;
} TypeQual;



 TypeQual MakeTQ (unsigned int tq, List *attributes);

 TypeQual tq_attribute (List *attrib);



 int tq_has_auto (TypeQual);
 int tq_has_extern (TypeQual);
 int tq_has_register (TypeQual);
 int tq_has_static (TypeQual);
 int tq_has_typedef (TypeQual);
 int tq_has_some_storageclass (TypeQual);

 TypeQual tq_add_auto (TypeQual);
 TypeQual tq_add_extern (TypeQual);
 TypeQual tq_add_register (TypeQual);
 TypeQual tq_add_static (TypeQual);
 TypeQual tq_add_typedef (TypeQual);

 TypeQual tq_storage_class (TypeQual);

 TypeQual tq_remove_all_storage_classes (TypeQual);
 TypeQual tq_set_storage_class_from (TypeQual, TypeQual);







 int tq_has_top_decl (TypeQual);
 int tq_has_block_decl (TypeQual);
 int tq_has_formal_decl (TypeQual);
 int tq_has_su_decl (TypeQual);
 int tq_has_enum_decl (TypeQual);

 TypeQual tq_add_top_decl (TypeQual);
 TypeQual tq_add_block_decl (TypeQual);
 TypeQual tq_add_formal_decl (TypeQual);
 TypeQual tq_add_su_decl (TypeQual);
 TypeQual tq_add_enum_decl (TypeQual);







 TypeQual tq_decl_location (TypeQual);
 TypeQual tq_set_decl_location_from (TypeQual tq, TypeQual source);
# 81 "../../include/typequal.h"
 int tq_has_redundant_external_decl (TypeQual);
 TypeQual tq_add_redundant_external_decl (TypeQual);
 TypeQual tq_remove_redundant_external_decl (TypeQual);


 int tq_has_nooutput (TypeQual);

 TypeQual tq_add_nooutput (TypeQual);



 TypeQual TQ_DECL_QUALS (void);
 TypeQual tq_decl_quals (TypeQual);


 TypeQual tq_add_restrict (TypeQual);
 TypeQual tq_add_complex (TypeQual);
# 109 "../../include/typequal.h"
 int tq_has_restrict (TypeQual);
 int tq_has_complex (TypeQual);
 int tq_has_const (TypeQual);
 int tq_has_volatile (TypeQual);
 int tq_has_inline (TypeQual);
 int tq_has_sue_elaborated (TypeQual);

 int tq_has_extension (TypeQual);

 TypeQual tq_add_restrict (TypeQual);
 TypeQual tq_add_complex (TypeQual);
 TypeQual tq_add_const (TypeQual);
 TypeQual tq_add_volatile (TypeQual);
 TypeQual tq_add_inline (TypeQual);
 TypeQual tq_add_sue_elaborated (TypeQual);
 TypeQual tq_add_extension (TypeQual);

 TypeQual tq_remove_complex (TypeQual);
 TypeQual tq_remove_const (TypeQual);
 TypeQual tq_remove_inline (TypeQual);
 TypeQual tq_remove_sue_elaborated (TypeQual);

 TypeQual tq_type_quals (TypeQual);
 TypeQual tq_remove_compatible (TypeQual);

 TypeQual tq_remove_all_decl_quals (TypeQual);

 TypeQual tq_union (TypeQual, TypeQual);
 TypeQual tq_intersection (TypeQual, TypeQual);
 TypeQual tq_subtract (TypeQual, TypeQual);

 Bool tq_has_anything (TypeQual);
 TypeQual tq_remove_everything (TypeQual);

 Bool tq_equal (TypeQual, TypeQual);


 void TQtoGBUF (GBUF *gb, TypeQual tq);


 void DPTQ (TypeQual tq);


# 6 "../../include/ast.h" 2
# 1 "../../include/wildcard.h" 1
       







typedef enum
{
  WildcardD,
  WildcardE,
  WildcardT,
  NonWildcard
} WildcardType;



 void PrintTypeLetter (FILE *out, WildcardType t);


 WildcardType WildcardTypeFromName (char const *wcName);





 List *GetWildcardTypes (char const *inputString);





 List *GetWildcardNames (char const *str);






 char *NameAllWildcards (char const *inputString);





 Bool TestWildcardLetterFunctions (void);




 Bool TestGetWildcardTypeCases (void);





 Bool TestGetWildcardNameCases (void);


 Bool TestWildcardLetterFunctions (void);




 Bool TestNameAllWildcards (void);


# 7 "../../include/ast.h" 2




typedef enum NodeType
{

# 1 "../../include/ast/types.h" 1

Const,
Id,
Binop,
Unary,
Cast,
Comma,
Constructor,
Ternary,
Array,
Call,
Initialiser,
ImplicitCast,


Label,
Switch,
Case,
Default,
If,
IfElse,
While,
Do,
For,
Goto,
Continue,
Break,
Return,
Block,


Prim,
Tdef,
Ptr,
Adcl,
Fdcl,
Sdcl,
Udcl,
Edcl,


Asm,
AsmArg,


BuiltinVaArg,


Decl,


Attrib,


Proc,


Text,
# 15 "../../include/ast.h" 2

} NodeType;
# 32 "../../include/ast.h"
typedef enum BasicType
{
  Unspecified = 0,

# 1 "../../include/ast/basictype.h" 1
Uchar,
Schar,
Char,

Sshort,
Ushort,

Sint,
Uint,

Int_ParseOnly,

Slong,
Ulong,

Slonglong,
Ulonglong,

Float,
Double,
Longdouble,

Void,
Ellipsis,
VaList,
# 37 "../../include/ast.h" 2

  MaxBasicType
} BasicType;
# 66 "../../include/ast.h"
typedef enum TypeSpecifier
{

  Short = 0x0100,
  Long = 0x0200,
  Longlong = 0x0300,
  LengthMask = Short | Long | Longlong,



  Signed = 0x0400,
  Unsigned = 0x0800,
  SignMask = Signed | Unsigned,






  BasicTypeMask = 0x00FF
} TypeSpecifier;


typedef struct ExplodedType
{
  TypeSpecifier base;
  TypeSpecifier sign;
  TypeSpecifier length;

} ExplodedType;
# 111 "../../include/ast.h"
typedef struct SUEtype
{
  NodeType typ;

  Bool complete;
  Coord coord;
  Coord right_coord;

  Bool visited;
  Bool transformed;
  int size;
  int align;

  char const *name;
  List *fields;
  List *attributes;
} SUEtype;
# 142 "../../include/ast.h"
typedef Node ChildNode;
typedef List ChildList;
# 156 "../../include/ast.h"
typedef struct ARRAYtype
{
  ChildNode *dim;
  int size;
} ARRAYtype;


typedef struct NodeBase
{
  NodeType typ;





  Coord coord;
# 181 "../../include/ast.h"
  List *pragmas;




  short pass;





  short parenthesised;
  int print_uid;


  Analysis analysis;


  short nondeterminator;

  short memorycheckedp;

  short memorywarningp;

  WildcardType wTyp;

  struct hv *self;
} NodeBase;
# 218 "../../include/ast.h"
typedef struct ConstNode
{
  NodeBase base;

  char const *text;


  ChildNode *type;


  union
  {
    TARGET_SINT i;
    TARGET_UINT u;
    TARGET_SLONG l;
    TARGET_ULONG ul;
    float f;
    double d;
    char const *s;
  } value;
} ConstNode;

typedef struct idNode
{
  NodeBase base;

  char const *text;
  Node *decl;

  Node *value;
} idNode;

typedef struct binopNode
{
  NodeBase base;

  OpType op;
  ChildNode *left;
  ChildNode *right;
  Node *type;
  Node *value;
} binopNode;

typedef struct unaryNode
{
  NodeBase base;

  OpType op;
  ChildNode *expr;
  Node *type;
  Node *value;
} unaryNode;

typedef struct castNode
{
  NodeBase base;

  ChildNode *type;
  ChildNode *expr;
  Node *value;
} castNode;

typedef struct constructorNode
{
  NodeBase base;

  ChildNode *type;
  ChildNode *initialiserlist;
} constructorNode;





typedef struct commaNode
{
  NodeBase base;

  ChildList *exprs;
} commaNode;

typedef struct ternaryNode
{
  NodeBase base;

  ChildNode *cond;
  ChildNode *iftrue;
  ChildNode *iffalse;
  Coord colon_coord;
  Node *type;
  Node *value;
} ternaryNode;

typedef struct arrayNode
{
  NodeBase base;

  Node *type;
  ChildNode *name;
  ChildList *dims;
} arrayNode;

typedef struct callNode
{
  NodeBase base;

  ChildNode *name;
  ChildList *args;
} callNode;

typedef struct initialiserNode
{
  NodeBase base;

  ChildList *exprs;
} initialiserNode;







typedef struct implicitcastNode
{
  NodeBase base;

  ChildNode *expr;
  Node *type;
  Node *value;
} implicitcastNode;
# 359 "../../include/ast.h"
typedef struct labelNode
{
  NodeBase base;

  char const *name;
  List *references;
  FlowValue label_values;
  Bool declared;
} labelNode;



typedef struct SwitchNode
{
  NodeBase base;

  ChildNode *expr;
  ChildNode *stmt;
  List *cases;
  Bool has_default;
  struct SwitchCheck *check;


  FlowValue switch_values, break_values;
} SwitchNode;

typedef struct CaseNode
{
  NodeBase base;

  ChildNode *expr;
  Node *container;
} CaseNode;

typedef struct DefaultNode
{
  NodeBase base;

  Node *container;
} DefaultNode;


typedef struct IfNode
{
  NodeBase base;

  ChildNode *expr;
  ChildNode *stmt;
} IfNode;

typedef struct IfElseNode
{
  NodeBase base;

  ChildNode *expr;
  ChildNode *iftrue;
  ChildNode *iffalse;
  Coord else_coord;
} IfElseNode;

typedef struct WhileNode
{
  NodeBase base;

  ChildNode *expr;
  ChildNode *stmt;
  FlowValue loop_values, break_values;
} WhileNode;

typedef struct DoNode
{
  NodeBase base;

  ChildNode *stmt;
  ChildNode *expr;
  Coord while_coord;
  FlowValue loop_values, continue_values, break_values;
} DoNode;

typedef struct ForNode
{
  NodeBase base;

  ChildNode *init;
  ChildNode *cond;
  ChildNode *next;
  ChildNode *stmt;
  ChildList *rwlist;
  FlowValue loop_values, continue_values, break_values;
} ForNode;

typedef struct GotoNode
{
  NodeBase base;

  Node *label;
} GotoNode;

typedef struct ContinueNode
{
  NodeBase base;

  Node *container;
} ContinueNode;

typedef struct BreakNode
{
  NodeBase base;

  Node *container;
} BreakNode;

typedef struct ReturnNode
{
  NodeBase base;

  ChildNode *expr;

  Node *proc;

  Bool needs_sync;




  List *livevars;
  List *dirtyvars;
} ReturnNode;

typedef struct BlockNode
{
  NodeBase base;

  ChildList *decl;
  ChildList *stmts;
  Coord right_coord;
  Node *type;



} BlockNode;
# 509 "../../include/ast.h"
typedef struct primNode
{
  NodeBase base;

  TypeQual tq;
  BasicType basic;
} primNode;

typedef struct tdefNode
{
  NodeBase base;

  char const *name;
  Node *decl;
  Node *type;
  TypeQual tq;
} tdefNode;

typedef struct ptrNode
{
  NodeBase base;

  TypeQual tq;
  ChildNode *type;
} ptrNode;

typedef struct adclNode
{
  NodeBase base;

  TypeQual tq;
  ChildNode *type;
  ARRAYtype *dimp;
} adclNode;

typedef struct fdclNode
{
  NodeBase base;

  TypeQual tq;
  ChildList *args;
  ChildNode *returns;
  Node *proc_node;
} fdclNode;

typedef struct sdclNode
{
  NodeBase base;

  TypeQual tq;
  SUEtype *type;
} sdclNode;

typedef struct udclNode
{
  NodeBase base;

  TypeQual tq;
  SUEtype *type;
} udclNode;

typedef struct edclNode
{
  NodeBase base;

  TypeQual tq;
  SUEtype *type;
} edclNode;
# 586 "../../include/ast.h"
typedef struct declNode
{
  NodeBase base;

  char const *name;
  TypeQual tq;
  ChildNode *type;
  ChildNode *init;


  ChildNode *bitsize;
  int references;
  List *attribs;
  ChildNode *asmdecl;

  Bool register_p;




  int kind;
# 620 "../../include/ast.h"
  char const *scope;
} declNode;

typedef struct attribNode
{
  NodeBase base;

  char const *name;
  ChildList *arglist;
} attribNode;

typedef struct procNode
{
  NodeBase base;

  ChildNode *decl;
  ChildNode *body;
  FlowValue return_values;
  Bool needs_sync;
  Bool needs_return;
} procNode;

typedef struct textNode
{
  NodeBase base;

  char const *text;
  Bool start_new_line;

} textNode;
# 659 "../../include/ast.h"
typedef struct AsmNode
{
  NodeBase base;

  TypeQual tq;
  ChildNode *tmplate;
  ChildList *output;
  ChildList *input;
  ChildList *clobbered;
} AsmNode;

typedef struct AsmArgNode
{
  NodeBase base;

  ChildNode *constraint;
  ChildNode *expr;
} AsmArgNode;
# 685 "../../include/ast.h"
typedef struct builtinvaargNode
{
  NodeBase base;

  ChildNode *expr;
  ChildNode *type;
} builtinvaargNode;
# 701 "../../include/ast.h"
union nodeStruct
{
  NodeBase base;

  ConstNode Const;
  idNode id;
  binopNode binop;
  unaryNode unary;
  castNode cast;
  commaNode comma;
  constructorNode constructor;
  ternaryNode ternary;
  arrayNode array;
  callNode call;
  initialiserNode initialiser;
  implicitcastNode implicitcast;
  builtinvaargNode builtinvaarg;
  labelNode label;
  SwitchNode Switch;
  CaseNode Case;
  DefaultNode Default;
  IfNode If;
  IfElseNode IfElse;
  WhileNode While;
  DoNode Do;
  ForNode For;
  GotoNode Goto;
  ContinueNode Continue;
  BreakNode Break;
  ReturnNode Return;
  BlockNode Block;
  primNode prim;
  tdefNode tdef;
  ptrNode ptr;
  adclNode adcl;
  fdclNode fdcl;
  sdclNode sdcl;
  udclNode udcl;
  edclNode edcl;

  AsmNode Asm;
  AsmArgNode AsmArg;
  declNode decl;
  attribNode attrib;
  procNode proc;
  textNode text;
};
# 831 "../../include/ast.h"







 Node *NewNode (NodeType typ);



 Node *MakeConstSint (int value);
 Node *MakeConstSintTextCoord (char const *text, int value, Coord coord);
 Node *MakeConstPtr (unsigned int value);
 Node *MakeConstPtrTextCoord (char const *text, unsigned int value, Coord coord);
 Node *MakeConstUint (unsigned int value);
 Node *MakeConstUintTextCoord (char const *text, unsigned int value, Coord coord);
 Node *MakeConstSlong (long value);
 Node *MakeConstSlongTextCoord (char const *text, long value, Coord coord);
 Node *MakeConstUlong (unsigned long value);
 Node *MakeConstUlongTextCoord (char const *text, unsigned long value, Coord coord);
 Node *MakeConstFloat (float value);
 Node *MakeConstFloatTextCoord (char const *text, float value, Coord coord);
 Node *MakeConstDouble (double value);
 Node *MakeConstDoubleTextCoord (char const *text, double value, Coord coord);
 Node *MakeString (char const *value);
 Node *MakeStringTextCoord (char const *text, char const *value, Coord coord);
 Node *MakeId (char const *text);
 Node *MakeIdCoord (char const *text, Coord coord);
 Node *MakeIdFromDecl (Node *decl);
 Node *MakeUnary (OpType op, Node *expr);
 Node *MakeUnaryCoord (OpType op, Node *expr, Coord coord);
 Node *MakeBinop (OpType op, Node *left, Node *right);
 Node *MakeBinopCoord (OpType op, Node *left, Node *right, Coord coord);
 Node *MakeCast (Node *type, Node *expr);
 Node *MakeCastCoord (Node *type, Node *expr, Coord coord);
 Node *MakeComma (List *exprs);
 Node *MakeCommaCoord (List *exprs, Coord coord);
 Node *MakeTernary (Node *cond, Node *iftrue, Node *iffalse);
 Node *MakeTernaryCoord (Node *cond, Node *iftrue, Node *iffalse, Coord qmark_coord, Coord colon_coord);
 Node *MakeArray (Node *name, List *dims);
 Node *MakeArrayCoord (Node *name, List *dims, Coord coord);
 Node *MakeCall (Node *name, List *args);
 Node *MakeCallCoord (Node *name, List *args, Coord coord);
 Node *MakeInitialiser (List *exprs);
 Node *MakeInitialiserCoord (List *exprs, Coord coord);
 Node *MakeImplicitCast (Node *type, Node *expr);
 Node *MakeImplicitCastCoord (Node *type, Node *expr, Coord coord);
 Node *MakeBuiltinVaArg (Node *type, Node *expr);
 Node *MakeBuiltinVaArgCoord (Node *type, Node *expr, Coord coord);
 Node *MakeLabel (char const *name);
 Node *MakeLabelCoord (char const *name, Coord coord);
 Node *MakeSwitch (Node *expr, Node *stmt, List *cases);
 Node *MakeSwitchCoord (Node *expr, Node *stmt, List *cases, Coord coord);
 Node *MakeCase (Node *expr, Node *container);
 Node *MakeCaseCoord (Node *expr, Node *container, Coord coord);
 Node *MakeDefault (Node *container);
 Node *MakeDefaultCoord (Node *container, Coord coord);
 Node *MakeIf (Node *expr, Node *stmt);
 Node *MakeIfCoord (Node *expr, Node *stmt, Coord coord);
 Node *MakeIfElse (Node *expr, Node *iftrue, Node *iffalse);
 Node *MakeIfElseCoord (Node *expr, Node *iftrue, Node *iffalse, Coord if_coord, Coord else_coord);
 Node *MakeWhile (Node *expr, Node *stmt);
 Node *MakeWhileCoord (Node *expr, Node *stmt, Coord coord);
 Node *MakeDo (Node *stmt, Node *expr);
 Node *MakeDoCoord (Node *stmt, Node *expr, Coord do_coord, Coord while_coord);
 Node *MakeFor (Node *init, Node *cond, Node *next, Node *stmt);
 Node *MakeForCoord (Node *init, Node *cond, Node *next, Node *stmt, Coord coord);
 Node *MakeGoto (Node *label);
 Node *MakeGotoCoord (Node *label, Coord coord);
 Node *MakeContinue (Node *container);
 Node *MakeContinueCoord (Node *container, Coord coord);
 Node *MakeBreak (Node *container);
 Node *MakeBreakCoord (Node *container, Coord coord);
 Node *MakeReturn (Node *expr);
 Node *MakeReturnCoord (Node *expr, Coord coord);
 Node *MakeBlock (Node *type, List *decl, List *stmts);
 Node *MakeBlockCoord (Node *type, List *decl, List *stmts, Coord left_coord, Coord right_coord);
 Node *MakePrim (TypeQual tq, BasicType basic);
 Node *MakePrimCoord (TypeQual tq, BasicType basic, Coord coord);
 Node *MakeTdef (TypeQual tq, char const *name);
 Node *MakeTdefCoord (TypeQual tq, char const *name, Coord coord);
 Node *MakePtr (TypeQual tq, Node *type);
 Node *MakePtrCoord (TypeQual tq, Node *type, Coord coord);
 Node *MakeAdcl (TypeQual tq, Node *type, Node *dim);
 Node *MakeAdclCoord (TypeQual tq, Node *type, Node *dim, Coord coord);
 Node *MakeFdcl (TypeQual tq, List *args, Node *returns);
 Node *MakeFdclCoord (TypeQual tq, List *args, Node *returns, Coord coord);
 Node *MakeSdcl (TypeQual tq, SUEtype *type);
 Node *MakeSdclCoord (TypeQual tq, SUEtype *type, Coord coord);
 Node *MakeUdcl (TypeQual tq, SUEtype *type);
 Node *MakeUdclCoord (TypeQual tq, SUEtype *type, Coord coord);
 Node *MakeEdcl (TypeQual tq, SUEtype *type);
 Node *MakeEdclCoord (TypeQual tq, SUEtype *type, Coord coord);
 Node *MakeDecl (char const *name, TypeQual tq, Node *type, Node *init, Node *bitsize);
 Node *MakeDeclCoord (char const *name, TypeQual tq, Node *type, Node *init, Node *bitsize, Coord coord);
 Node *MakeAttrib (char const *name, ChildList *arglist);
 Node *MakeAttribCoord (char const *name, ChildList *argl, Coord coord);
 Node *MakeProc (Node *decl, Node *body);
 Node *MakeProcCoord (Node *decl, Node *body, Coord coord);
 Node *MakeText (char const *text, Bool start_new_line);
 Node *MakeTextCoord (char const *text, Bool start_new_line, Coord coord);


 Node *MakeConstructor (Node *type, Node *initialiserlist);
 Node *MakeConstructorCoord (Node *type, Node *initialiserlist, Coord coord);

 Node *MakeAsm (TypeQual tq, Node *tmplate, List *output, List *input, List *clobbered);
 Node *MakeAsmCoord (TypeQual tq, Node *tmplate, List *output, List *input, List *clobbered, Coord coord);

 Node *MakeAsmArg (Node *constraint, Node *expr);
 Node *MakeAsmArgCoord (Node *constraint, Node *expr, Coord coord);





 Node *MakeWildcardId (char const *text);
 Node *MakeWildcardIdCoord (char const *text, Coord coord);

 Node *MakeWildcardExp (char const *text);
 Node *MakeWildcardExpCoord (char const *text, Coord coord);

 Node *MakeWildcardType (char const *text);
 Node *MakeWildcardTypeCoord (char const *text, Coord coord);


 Node *ConvertIdToTdef (Node *id, TypeQual tq, Node *decl, Node *type);
 Node *ConvertIdToDecl (Node *id, TypeQual tq, Node *type, Node *init, Node *bitsize);
 Node *ConvertIdToAttrib (Node *id, ChildList *arg);
# 969 "../../include/ast.h"
typedef enum
{
  NodeOnly,
  Subtree,
  NodeOnlyExact,
  SubtreeExact
} TreeOpDepth;

typedef enum
{
  Preorder,
  Postorder
} WalkOrder;

typedef void WalkProc (Node *, void *);

 Node *NodeCopy (Node *from, TreeOpDepth d);
 Node *SetCoords (Node *tree, Coord c, TreeOpDepth d);

 void WalkTree (Node *tree, WalkProc proc, void *ptr, WalkOrder order);
 void PrintCoord (GBUF *out, Coord *c);



 List *ListCopyNodes (List *, TreeOpDepth d);





 Bool IsExpr (Node *node);
 Bool IsStmt (Node *node);
 Bool IsType (Node *node);
 Bool IsDecl (Node *node);


 Bool IsWildcard (Node *node);
 Bool IsWildcardD (Node *node);

typedef int Kinds;





 Kinds KindsOfNode (Node *node);







# 1 "../../include/operators.h" 1
       




typedef struct
{
  char const *text;
  char const *name;
  short unary_prec;
  short binary_prec;
  Bool left_assoc;
  Bool (*unary_eval)(Node *);
  Bool (*binary_eval)(Node *);
} OpEntry;

 extern OpEntry Operator[600];


 void InitOperatorTable (void);
 int OpPrecedence (NodeType typ, OpType op, Bool *left_assoc);


# 1023 "../../include/ast.h" 2

# 1 "./yyparse.h" 1
# 40 "./yyparse.h"
extern int yydebug;







   enum yytokentype {
     AUTO = 258,
     BREAK = 259,
     CASE = 260,
     CHAR = 261,
     CONST = 262,
     CONTINUE = 263,
     DEFAULT = 264,
     DO = 265,
     DOUBLE = 266,
     ELSE = 267,
     ENUM = 268,
     EXTERN = 269,
     FLOAT = 270,
     FOR = 271,
     GOTO = 272,
     IF = 273,
     INT = 274,
     LONG = 275,
     REGISTER = 276,
     RETURN = 277,
     SHORT = 278,
     SIGNED = 279,
     SIZEOF = 280,
     STATIC = 281,
     STRUCT = 282,
     SWITCH = 283,
     TYPEDEF = 284,
     UNION = 285,
     UNSIGNED = 286,
     VOID = 287,
     VOLATILE = 288,
     WHILE = 289,
     UPLUS = 290,
     UMINUS = 291,
     INDIR = 292,
     ADDRESS = 293,
     POSTINC = 294,
     POSTDEC = 295,
     PREINC = 296,
     PREDEC = 297,
     BOGUS = 298,
     IDENTIFIER = 299,
     STRINGliteral = 300,
     FLOATINGconstant = 301,
     INTEGERconstant = 302,
     OCTALconstant = 303,
     HEXconstant = 304,
     WIDECHARconstant = 305,
     CHARACTERconstant = 306,
     COMPLEX = 307,
     REAL = 308,
     IMAG = 309,
     TYPEDEFname = 310,
     ARROW = 311,
     ICR = 312,
     DECR = 313,
     LS = 314,
     RS = 315,
     LE = 316,
     GE = 317,
     EQ = 318,
     NE = 319,
     ANDAND = 320,
     OROR = 321,
     ELLIPSIS = 322,
     MULTassign = 323,
     DIVassign = 324,
     MODassign = 325,
     PLUSassign = 326,
     MINUSassign = 327,
     LSassign = 328,
     RSassign = 329,
     ANDassign = 330,
     ERassign = 331,
     ORassign = 332,
     INLINE = 333,
     ATTRIBUTE = 334,
     EXTENSION = 335,
     RESTRICT = 336,
     ALIGNOF = 337,
     TYPEOF = 338,
     BUILTIN_VA_ARG = 339,
     BUILTIN_VA_LIST = 340,
     DESIGNATED_INITIALISER = 341,
     ARRAY_LABELED_INITIALISER = 342,
     WILDCARD_T = 343,
     WILDCARD_E = 344,
     WILDCARD_D = 345,
     ASM = 346,
     SHIFT_THERE = 347
   };




typedef union YYSTYPE
{
# 114 "yyparse.y"

  Node *n;
  List *L;


  struct
  {
    TypeQual tq;
    Coord coord;
  } tq;


  Coord tok;
# 165 "yyparse.h"
} YYSTYPE;






typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# 193 "yyparse.h"
int yyparse (void);
# 1025 "../../include/ast.h" 2


union YYSTYPE;
struct YYLTYPE;

 int yyparse (void);
 int yylex (union YYSTYPE *lval, struct YYLTYPE *lloc);


 void PushContainer (NodeType typ);
 Node *PopContainer (Node *n);
 Node *AddContainee (Node *c);


# 1 "../../include/type.h" 1
       



 extern Node *EllipsisNode;


typedef enum
{
  Redecl,
  NoRedecl,
  SU,
  Formal

} ScopeState;


 extern Node *PrimVoid;
 extern Node *PrimVaList;
 extern Node *PrimChar;
 extern Node *PrimSchar;
 extern Node *PrimUchar;
 extern Node *PrimSshort;
 extern Node *PrimUshort;
 extern Node *PrimSint;
 extern Node *PrimUint;
 extern Node *PrimSlong;
 extern Node *PrimUlong;
 extern Node *PrimSlonglong;
 extern Node *PrimUlonglong;
 extern Node *PrimFloat;
 extern Node *PrimDouble;
 extern Node *PrimLongdouble;

 extern Node *StaticString;

 extern Node *SintZero;
 extern Node *UintZero;
 extern Node *SlongZero;
 extern Node *UlongZero;
 extern Node *FloatZero;
 extern Node *DoubleZero;

 extern Node *PtrVoid;
 extern Node *PtrNull;

 extern Node *SintOne;
 extern Node *UintOne;
 extern Node *SlongOne;
 extern Node *UlongOne;
 extern Node *FloatOne;
 extern Node *DoubleOne;



 extern Bool OldStyleFunctionDefinition;







 extern Node *Undeclared;


 void InitTypes (void);

typedef TypeQual TqUpdate (TypeQual);
typedef TypeQual TqUpdate2 (TypeQual, TypeQual);

 Node *TypeQualifyNode (Node *node, TypeQual tq, Coord const *tq_coord);
 TypeQual MergeTypeQuals (TypeQual left, TypeQual right, Coord coord);
 Node *MakeMergedType (Node *type1, Node *qual1);
 TypeQual NodeTq (Node *node);
 void NodeUpdateTq (Node * node, TqUpdate f);
 void NodeUpdateTq2 (Node * node, TqUpdate2 f, TypeQual tq2);
 TypeQual NodeTypeQuals (Node *node);
 TypeQual NodeDeclTq (Node *node);
 TypeQual NodeStorageClass (Node *node);
 void NodeSetStorageClass (Node *node, TypeQual sc);
 TypeQual NodeDeclLocation (Node *node);
 void NodeSetDeclLocation (Node *node, TypeQual dl);


 void TQtoText (char array[], int arraysize, TypeQual tq);

 void PrintTQ (GBUF *out, TypeQual tq, int offset, Bool norecurse);

 Node *StartPrimType (BasicType basic, Coord coord);
 Node *MergePrimTypes (Node *node, Node *node2);
 Node *FinishPrimType (Node *PrimNode);
 Node *MakeDefaultPrimType (TypeQual tq, Coord coord);
 Node *SansSign (Node *type);


 Node *LookupFunction (Node *call);
 Node *LookupPostfixExpression (Node *post);
 Bool IsAType (char const *name);
 Node *GetTypedefType (Node *id);
 Node *GetTypedefDecl (Node *id);
 ExitscopeProc OutOfScope;

 unsigned int NodeSizeof (Node *node, Node *node_type, Bool allow_unsized_array);
 int NodeAlignment (Node *node, Node *node_type);

 Node *ArrayRefType (Node *atype, List *dims);
 Bool IsObjectType (Node *node);
 Bool IsFunctionType (Node *node);
 Bool IsIncompleteType (Node *node);
 Bool IsVoidType (Node *node);
 Bool IsArrayType (Node *node);
 Bool IsSueType (Node *node);
 Bool IsStructType (Node *node);
 Bool IsUnionType (Node *node);
 Bool IsEnumType (Node *node);
 Bool IsPointerType (Node *node);
 Bool IsScalarType (Node *node);
 Bool IsArithmeticType (Node *node);
 Bool IsIntegralType (Node *node);
 Bool IsFloatingType (Node *node);
 Bool IsLvalue (Node *node);
 Bool IsModifiableLvalue (Node *node);
 Bool IsVoidArglist (List *arglist);
 Bool IsEllipsis (Node *node);
 Bool IsRelationalExpression (Node *node);
 Bool IsLogicalExpression (Node *node);
 Bool IsLogicalOrRelationalExpression (Node *node);
 Bool IsPtrToObject (Node *node);
 Bool IsPtrToVoid (Node *node);
 Bool IsPtrToFunction (Node *node);
 Node *ArrayType (Node *array);
 Bool IsStructType (Node *node);
 Node *PtrSubtype (Node *node);
 Bool DeclIsExtern (Node *node);
 Bool DeclIsEnumConst (Node *node);
 Bool DeclIsTypedef (Node *node);
 Bool DeclIsBlock (Node *node);
 Bool NodeIsConstQual (Node *node);
 Bool IsUnsizedArray (Node *node);
 Bool IsStructComplete (Node *node);
 Bool IsUnionComplete (Node *node);
 Bool TypeIsChar (Node *type);
 Bool TypeIsSint (Node *type);
 Bool TypeIsUint (Node *type);
 Bool TypeIsSlong (Node *type);
 Bool TypeIsUlong (Node *type);
 Bool TypeIsFloat (Node *type);
 Bool TypeIsDouble (Node *type);
 Bool TypeIsString (Node *type);
 Bool NodeTypeIsChar (Node *node);
 Bool NodeTypeIsSint (Node *node);
 Bool NodeTypeIsUint (Node *node);
 Bool NodeTypeIsSlong (Node *node);
 Bool NodeTypeIsUlong (Node *node);
 Bool NodeTypeIsFloat (Node *node);
 Bool NodeTypeIsDouble (Node *node);
 Bool NodeTypeIsString (Node *node);
 Bool NodeTypeIsIntegral (Node *node);
 Bool NodeTypeIsUnsizedArray (Node *node);
 Bool IsPrimChar (Node *node);
 Bool IsPrimCharModuloSign (Node *node);
 Bool IsArrayOfChar (Node *node);
 Bool IsArrayOfCharModuloSign (Node *node);
 Bool IsStringConstant (Node *node);
 Bool IsAggregateType (Node *node);
 Bool DeclIsStatic (Node *node);
 Bool DeclIsExternal (Node *node);
 Node *FunctionReturnType (Node *node);
 void FunctionSetReturnType (Node *node, Node *new_type);
 void FunctionPushArglist (Node *node, Node *new_arg);
 Bool IsPrimitiveStmt (Node *node);

 Bool TypeEqual (Node *type1, Node *type2);
 Bool TypeEqualFormals (Node *type1, Node *type2);
 Bool TypeEqualQualified (Node *type1, Node *type2, Bool strict_toplevel, Bool strict_recursive);



 Node *SetBaseType (Node *complex, Node *base);
 Node *GetShallowBaseType (Node *complex);
 Node *GetDeepBaseType (Node *complex);
 Bool DeepBaseTypeIsComplete (Node *complex);
 Node *ExtendArray (Node *array, Node *dim, Coord coord);
 Node *ModifyDeclType (Node *decl, Node *modifier);
 Node *SetDeclType (Node *decl, Node *type, ScopeState redeclare);
 Node *SetDeclInit (Node *decl, Node *init);
 Node *SetDeclBitSize (Node *decl, Node *bitsize);
 Node *SetDeclAttribs (Node * decl, TypeQual);
 Node *SetDeclAsm (Node *decl, Node *asmnode);
 List *AppendDecl (List *list, Node *decl, ScopeState redeclare);
 Node *FinishType (Node *type);
 Node *FinishDecl (Node *type);



 void PrintSUE (GBUF *out, SUEtype *sue, int offset, Bool norecurse);
 Node *SetSUdclFields (Node *sudcl, Node *id, List *fields, Coord left_coord, Coord right_coord, List *attributes);
 Node *SetSUdclNameFields (Node *sudcl, Node *id, List *fields, Coord left_coord, Coord right_coord, List *attributes);
 Node *SetSUdclName (Node *sudcl, Node *id, Coord coord);
 Node *BuildEnum (Node *id, List *values, Coord enum_coord, Coord left_coord, Coord right_coord);
 Node *BuildEnumConst (Node *id, Node *value);
 ShadowProc ShadowTag;
 void VerifySUEcomplete (Node *type);
 Node *ForceNewSU (Node *sudcl, Coord coord);
 int SUE_Sizeof (SUEtype *sue);
 int SUE_Alignment (SUEtype *sue);
 Node *SUE_FindField (SUEtype *sue, Node *field_name);
 Bool SUE_SameTagp (SUEtype *sue1, SUEtype *sue2);



 Node *AddParameterTypes (Node *decl, List *types);
 Node *AddDefaultParameterTypes (Node *decl);
 Node *DefineProc (Bool old_style, Node *decl);
 Node *DemoteProcArgs (Node *decl);
 Node *SetProcBody (Node *proc, Node *block);
 Node *AddReturn (Node *returnnode);
 ConflictProc FunctionConflict;
 Node *BuildLabel (Node *id);
 Node *ResolveGoto (Node *id, Coord coord);
 ExitscopeProc EndOfLabelScope;
 void MaybeCheckLabelAtEndOfCompoundStatement (List *);


 Node *NodeDataType (Node *node);
 Node *NodeDataTypeSuperior (Node *node);
 void SetNodeDataType (Node *node, Node *type);
 Node *SdclFindField (Node *sdcl, Node *field_name);
 char const *TypeName (TypeSpecifier);


 char const *OperatorName (OpType op);
 char const *OperatorText (OpType op);
 Bool IsAssignmentOp (OpType op);
 Bool IsIncDecOp (OpType op);
 Bool IsComparisonOp (OpType op);
 Bool IsArithmeticOp (OpType op);

 OpType OpOfAssignmentOp (OpType op);




 Bool NodeIsConstant (Node *node);
 Node *NodeGetConstantValue (Node *node);
 void NodeSetConstantValue (Node *node, Node *value);

 int NodeConstantCharValue (Node *node);
 int NodeConstantSintValue (Node *node);
 unsigned int NodeConstantUintValue (Node *node);
 long NodeConstantSlongValue (Node *node);
 unsigned long NodeConstantUlongValue (Node *node);
 float NodeConstantFloatValue (Node *node);
 double NodeConstantDoubleValue (Node *node);
 char const *NodeConstantStringValue (Node *node);
 unsigned long NodeConstantIntegralValue (Node *node);
 Bool NodeConstantBooleanValue (Node *node);

 void NodeSetSintValue (Node *node, int i);
 void NodeSetUintValue (Node *node, unsigned u);
 void NodeSetSlongValue (Node *node, long l);
 void NodeSetUlongValue (Node *node, unsigned long ul);
 void NodeSetFloatValue (Node *node, float f);
 void NodeSetDoubleValue (Node *node, double d);
 void NodeSetStringValue (Node *node, char const *s);

 Bool IsConstantZero (Node *node);
 Bool IsConstantString (Node *node);
 Bool IsIntegralConstant (Node *node);
 Bool IntegralConstEqual (Node *value1, Node *value2);

 void ConstFoldTernary (Node *node);
 void ConstFoldCast (Node *node);


# 1040 "../../include/ast.h" 2

 List *GrabPragmas (List *stmts_or_decls);


 void DualASTWalk ( Node *node1, Node *node2, void *answer
                        , void code (Node *n1, Node *n2, void *answer)
                        , Bool continue_code (Node *n1, Node *n2, void *answer)
                        , void fail_code (void *answer)
                        );
 void ASTWalkTwo ( Node *node1, Node *node2, void *answer
                       , void code (Node *n1, Node *n2, void *answer)
                       , Bool continue_code (Node *n1, Node *n2, void *answer)
                       , void fail_code (void *answer)
                       );


 char const *GetNodeName (Node *n);
 char const *GetWildcardName (Node *wildcardNode);





 SUEtype *make_SUE (NodeType typ, char const *name, List *fields);


# 2 "sem-check.cx" 2

# 1 "../../include/conversions.h" 1
       



 Node *UsualUnaryConversions (Node *node, Bool f_to_d);
 Node *UsualUnaryConversionType (Node *type);
 void UsualBinaryConversions (Node **node1p, Node **node2p);
 void UsualPointerConversions (Node **node1p, Node **node2p, Bool allow_void_or_zero);
 Node *AssignmentConversions (Node *expr, Node *to_type);
 Node *CallConversions (Node *expr, Node *to_type);
 Node *ReturnConversions (Node *expr, Node *to_type);
 Node *CastConversions (Node *expr, Node *to_type);
 Node *ConditionalConversions (Node **truep, Node **falsep);


# 4 "sem-check.cx" 2
# 1 "../../include/initialiser.h" 1
       



 void SemCheckDeclInit (Node *decl, Bool blockp);
 Bool IsInitialiser (Node *node);
 Node *InitialiserCopy (Node *node);
 int InitialiserLength (Node *node);
 Node *InitialiserFirstItem (Node *node);
 List *InitialiserExprs (Node *node);
 Bool InitialiserEmptyList (Node *node);
 void InitialiserNext (Node *node);
 Node *InitialiserAppendItem (Node *initialiser, Node *element);
 Node *ArraySubtype (Node *node);
 int ArrayNumberElements (Node *node);
 SUEtype *StructUnionFields (Node *node);
 Node *UnionFirstField (Node *node);
 Node *SUE_MatchInitList (SUEtype *sue, Node *decl, Node *initialiser, Bool top_p);


# 5 "sem-check.cx" 2

static void SemCheckIsArithmeticType (Node *type, Node *op);
static void SemCheckIsScalarType (Node *type, Node *op);
static void SemCheckIsIntegralType (Node *type, Node *op);
static void AssignEnumValues (SUEtype *enm);
 void StructCheckFields (SUEtype *sue);
 void UnionCheckFields (SUEtype *sue);

static Node *SemCheckAssignment (Node *node, binopNode *u);
static Node *SemCheckDot (Node *node, binopNode *u);
static Node *SemCheckDesignatedInitialiser (Node *node, binopNode *u);
static Node *SemCheckArrayLabeledInitialiser (Node *node, binopNode *u);
static Node *SemCheckArrow (Node *node, binopNode *u);
static Node *SemCheckArithmetic (Node *node, binopNode *u);
static Node *SemCheckComparison (Node *node, binopNode *u);

static void SemCheckCallArgs (Node *node, List *formals, List *actuals);

static struct SwitchCheck *NewSwitchCheck (List *cases);
static void FreeSwitchCheck (struct SwitchCheck *check);
static void SwitchCheckAddCase (struct SwitchCheck *check, Node *expr);
static void SwitchCheckAddDefault (struct SwitchCheck *check, Node *node);

 List *
SemanticCheckProgram (List *program)
{
  ListMarker marker;
  Node *item;

  IterateList (&marker, program);
  while (NextOnList (&marker, (GenericREF) & item))
    {
      ((item) ? (void) (0) : __assert_fail ("item", "sem-check.cx", 37, "<func>"));
      SemCheckNode (item);

      if (item->base.typ == Decl && item->decl.init == ((void *)0))
        {
          Node *type;

          ((item->decl.type) ? (void) (0) : __assert_fail ("item->decl.type", "sem-check.cx", 44, "<func>"));
          type = NodeDataType (item->decl.type);

          if (NodeIsConstQual (type) && !DeclIsExtern (item) && !DeclIsTypedef (item))
            WarningCoord (1, item->base.coord,
                          "const object should have initialiser: \"%s\"",
                          item->decl.name);
        }
    }

  return program;
}
# 65 "sem-check.cx"
static Node *
SemCheckConst (Node *node, ConstNode *u)
{
  u->type = SemCheckNode (u->type);
  return node;
}

static Node *
SemCheckId (Node *node, idNode *u)
{
  if (u->decl)
    {
      Node *decl = u->decl;


      if (DeclIsEnumConst (decl))
        {
          ((decl->decl.init) ? (void) (0) : __assert_fail ("decl->decl.init", "sem-check.cx", 82, "<func>"));
          NodeSetConstantValue (node, NodeGetConstantValue (decl->decl.init));
        }
    }
  return node;
}

static Node *
SemCheckBinop (Node *node, binopNode *u)
{
  OpType op = u->op;

  u->left = SemCheckNode (u->left);
  u->right = SemCheckNode (u->right);

  if (IsAssignmentOp (op))
    return SemCheckAssignment (node, u);
  else if (op == '.')
    return SemCheckDot (node, u);
  else if (op == ARROW)
    return SemCheckArrow (node, u);
  else if (op == ':' || op == DESIGNATED_INITIALISER)
    return SemCheckDesignatedInitialiser (node, u);
  else if (op == ARRAY_LABELED_INITIALISER)
    return SemCheckArrayLabeledInitialiser (node, u);
  else if (IsArithmeticOp (op))
    return SemCheckArithmetic (node, u);
  else if (IsComparisonOp (op))
    return SemCheckComparison (node, u);
  else
    {
      fprintf (stderr, "Internal Error: Unrecognised Binop\n");
      (((1 == 0)) ? (void) (0) : __assert_fail ("(1 == 0)", "sem-check.cx", 114, "<func>"));
    }
  Fail ("sem-check.cx", 116, "UNREACHABLE");
  return ((void *)0);
}

static Node *
SemCheckUnary (Node *node, unaryNode *u)
{
  u->expr = SemCheckNode (u->expr);
  u->type = NodeDataType (u->expr);

  switch (u->op)
    {

    case UMINUS:
      SemCheckIsArithmeticType (u->type, node);

      if (NodeIsConstant (u->expr))
        {
          if (NodeTypeIsSint (u->expr))
            {
              int eval = NodeConstantSintValue (u->expr);

              NodeSetSintValue (node, 0 - eval);
            }
          else if (NodeTypeIsUint (u->expr))
            {
              unsigned int eval = NodeConstantUintValue (u->expr);

              NodeSetUintValue (node, 0 - eval);
            }
          else if (NodeTypeIsSlong (u->expr))
            {
              long eval = NodeConstantSlongValue (u->expr);

              NodeSetSlongValue (node, 0 - eval);
            }
          else if (NodeTypeIsUlong (u->expr))
            {
              unsigned long eval = NodeConstantUlongValue (u->expr);

              NodeSetUlongValue (node, 0 - eval);
            }
          else if (NodeTypeIsFloat (u->expr))
            {
              float eval = NodeConstantFloatValue (u->expr);

              NodeSetFloatValue (node, 0 - eval);
            }
          else if (NodeTypeIsDouble (u->expr))
            {
              double eval = NodeConstantDoubleValue (u->expr);

              NodeSetDoubleValue (node, 0 - eval);
            }
        }
      break;
    case UPLUS:
      SemCheckIsArithmeticType (u->type, node);
      return u->expr;

    case '!':
      u->expr = UsualUnaryConversions (u->expr, (1 == 0));
      u->type = NodeDataType (u->expr);
      u->type = PrimSint;

      if (NodeIsConstant (u->expr))
        {
          if (NodeTypeIsSint (u->expr))
            {
              int eval = NodeConstantSintValue (u->expr);

              NodeSetSintValue (node, eval == 0);
            }
          else if (NodeTypeIsUint (u->expr))
            {
              unsigned int eval = NodeConstantUintValue (u->expr);

              NodeSetSintValue (node, eval == 0);
            }
          else if (NodeTypeIsSlong (u->expr))
            {
              long eval = NodeConstantSlongValue (u->expr);

              NodeSetSintValue (node, eval == 0);
            }
          else if (NodeTypeIsUlong (u->expr))
            {
              unsigned long eval = NodeConstantUlongValue (u->expr);

              NodeSetSintValue (node, eval == 0);
            }
          else if (NodeTypeIsFloat (u->expr))
            {
              float eval = NodeConstantFloatValue (u->expr);

              NodeSetSintValue (node, eval == 0);
            }
          else if (NodeTypeIsDouble (u->expr))
            {
              double eval = NodeConstantDoubleValue (u->expr);

              NodeSetSintValue (node, eval == 0);
            }
        }
      break;


    case '~':
      u->expr = UsualUnaryConversions (u->expr, (1 == 0));
      u->type = NodeDataType (u->expr);
      SemCheckIsIntegralType (u->type, node);
      if (NodeIsConstant (u->expr))
        {
          if (NodeTypeIsSint (u->expr))
            {
              int eval = NodeConstantSintValue (u->expr);

              NodeSetSintValue (node, ~eval);
            }
          else if (NodeTypeIsUint (u->expr))
            {
              unsigned int eval = NodeConstantUintValue (u->expr);

              NodeSetUintValue (node, ~eval);
            }
          else if (NodeTypeIsSlong (u->expr))
            {
              long eval = NodeConstantSlongValue (u->expr);

              NodeSetSlongValue (node, ~eval);
            }
          else if (NodeTypeIsUlong (u->expr))
            {
              unsigned long eval = NodeConstantUlongValue (u->expr);

              NodeSetUlongValue (node, ~eval);
            }
        }
      break;


    case PREINC:
    case PREDEC:
    case POSTINC:
    case POSTDEC:
      if (!IsModifiableLvalue (u->expr))
        SyntaxErrorCoord (node->base.coord,
                          "operand must be modifiable lvalue: op %s",
                          Operator[u->op].text);

      u->type = NodeDataType (u->expr);
      SemCheckIsScalarType (u->type, node);

      if (IsPointerType (u->type))



        (void)NodeSizeof (node, GetShallowBaseType (u->type), (1 == 0));
      break;


    case SIZEOF:
      NodeSetUintValue (node, NodeSizeof (u->expr, u->type, (1 == 0)));
      u->type = PrimUint;
      break;

    case ALIGNOF:
      u->type = PrimUint;
      break;

    case IMAG:
    case REAL:
      u->type = NodeCopy (NodeDataType (u->expr), NodeOnly);


      NodeUpdateTq (u->type, tq_remove_complex);
      break;


    case ADDRESS:
      if (IsLvalue (u->expr) || IsFunctionType (u->type) || IsArrayType (u->type))
        u->type = MakePtr (((TypeQual) { 0, ((void *)0) }), u->type);
      else
        SyntaxErrorCoord (node->base.coord,
                          "cannot take the address of a non-lvalue");

      break;


    case INDIR:
      if (u->type->base.typ == Ptr)
        u->type = u->type->ptr.type;
      else if (u->type->base.typ == Adcl)
        u->type = u->type->adcl.type;
      else if (u->type->base.typ == Fdcl)




        ;
      else
        SyntaxErrorCoord (node->base.coord, "cannot dereference non-pointer type");
      break;

    default:
      fprintf (stdout,
               "Unsupported unary operator \"%s\"\n",
               Operator[u->op].text);
      (((1 == 0)) ? (void) (0) : __assert_fail ("(1 == 0)", "sem-check.cx", 324, "<func>"));
    }

  return node;
}

static Node *
SemCheckCast (Node *node, castNode *u)
{
  u->type = SemCheckNode (u->type);
  u->expr = SemCheckNode (u->expr);

  u->expr = CastConversions (u->expr, u->type);
  ConstFoldCast (node);
  return node;
}

static Node *
SemCheckComma (Node *node, commaNode *u)
{
  u->exprs = SemCheckList (u->exprs);
  return node;
}

static Node *
SemCheckConstructor (Node *node, constructorNode *u)
{
  u->type = SemCheckNode (u->type);
  u->initialiserlist = SemCheckNode (u->initialiserlist);
  return node;
}

static Node *
SemCheckTernary (Node *node, ternaryNode *u)
{
  Node *cond = u->cond, *iftrue = u->iftrue, *iffalse = u->iffalse, *ctype,
  *ttype, *ftype;

  cond = SemCheckNode (cond);
  iftrue = SemCheckNode (iftrue);
  iffalse = SemCheckNode (iffalse);

  cond = UsualUnaryConversions (cond, (1 == 0));

  ((cond) ? (void) (0) : __assert_fail ("cond", "sem-check.cx", 368, "<func>"));




  ((iffalse) ? (void) (0) : __assert_fail ("iffalse", "sem-check.cx", 373, "<func>"));

  ctype = NodeDataType (cond);
  if (iftrue)
    ttype = NodeDataType (iftrue);
  ftype = NodeDataType (iffalse);


  if (iftrue)
    iftrue = UsualUnaryConversions (iftrue, (1 == 0));
  iffalse = UsualUnaryConversions (iffalse, (1 == 0));


  if (iftrue)
    {
      u->type = ConditionalConversions (&iftrue, &iffalse);
      u->iftrue = iftrue;
      u->iffalse = iffalse;
    }
  else
    {
      u->type = ConditionalConversions (&cond, &iffalse);
      u->cond = cond;
      u->iffalse = iffalse;
    }
  ConstFoldTernary (node);

  return node;
}

static Node *
SemCheckArray (Node *node, arrayNode *u)
{
  Node *type;

  ((u->name) ? (void) (0) : __assert_fail ("u->name", "sem-check.cx", 408, "<func>"));
  ((u->dims) ? (void) (0) : __assert_fail ("u->dims", "sem-check.cx", 409, "<func>"));

  u->name = SemCheckNode (u->name);
  SemCheckList (u->dims);

  ((u->name) ? (void) (0) : __assert_fail ("u->name", "sem-check.cx", 414, "<func>"));
  type = NodeDataType (u->name);


  if (type->base.typ != Adcl && type->base.typ != Ptr)
    {
      Node *first = FirstItem (u->dims), *ftype;


      ((first) ? (void) (0) : __assert_fail ("first", "sem-check.cx", 423, "<func>"));
      ftype = NodeDataType (first);

      if (ftype->base.typ == Adcl || ftype->base.typ == Ptr)
        {
          SetItem (u->dims, u->name);
          u->name = first;
          type = ftype;
        }
      else
        {
          SyntaxErrorCoord (node->base.coord,
                            "cannot dereference non-pointer type");
          u->type = PrimVoid;
          return node;
        }
    }


  NodeSizeof (u->name, GetShallowBaseType (type), (1 == 0));
# 451 "sem-check.cx"
  u->type = ArrayType (node);

  return node;
}

static Node *
SemCheckCall1 (Node *node, callNode *u, Node *call_type)
{
  if (call_type->base.typ == Fdcl)
    SemCheckCallArgs (node, call_type->fdcl.args, u->args);
  else if (call_type->base.typ == Ptr)
    return SemCheckCall1 (node, u, NodeDataType (call_type->ptr.type));
  else
    SyntaxErrorCoord (node->base.coord, "called object is not a function");

  return node;
}

static Node *
SemCheckCall (Node *node, callNode *u)
{
  Node *call_type;

  u->name = SemCheckNode (u->name);
  u->args = SemCheckList (u->args);

  call_type = NodeDataType (u->name);

  return SemCheckCall1 (node, u, call_type);
}

static Node *
SemCheckInitialiser (Node *node, initialiserNode *u)
{
  u->exprs = SemCheckList (u->exprs);
  return node;
}

static Node *
SemCheckImplicitCast (Node *node, implicitcastNode *u)
{
  u->expr = SemCheckNode (u->expr);
  return node;
}
# 504 "sem-check.cx"
static Node *
SemCheckLabel (Node *node, labelNode * u)
{
  return node;
}

static Node *
SemCheckSwitch (Node *node, SwitchNode *u)
{
  u->expr = SemCheckNode (u->expr);
  u->expr = UsualUnaryConversions (u->expr, (1 == 0));

  if (!NodeTypeIsIntegral (u->expr))
    SyntaxErrorCoord (u->expr->base.coord,
                      "controlling expression must have integral type");

  u->check = NewSwitchCheck (u->cases);
  u->stmt = SemCheckNode (u->stmt);





  FreeSwitchCheck (u->check);
  u->check = ((void *)0);

  return node;
}

static Node *
SemCheckCase (Node *node, CaseNode *u)
{
  u->expr = SemCheckNode (u->expr);
  u->expr = UsualUnaryConversions (u->expr, (1 == 0));

  if (!IsIntegralConstant (u->expr))
    SyntaxErrorCoord (u->expr->base.coord,
                      "case expression must be integer constant");
  else
    {
      ((u->container->base.typ == Switch) ? (void) (0) : __assert_fail ("u->container->base.typ == Switch", "sem-check.cx", 544, "<func>"));
      SwitchCheckAddCase (u->container->Switch.check, u->expr);
    }

  return node;
}

static Node *
SemCheckDefault (Node *node, DefaultNode *u)
{
  ((u->container->base.typ == Switch) ? (void) (0) : __assert_fail ("u->container->base.typ == Switch", "sem-check.cx", 554, "<func>"));
  SwitchCheckAddDefault (u->container->Switch.check, node);

  return node;
}

static Node *
EnsureScalarType (Node *node)
{
  Node *type;

  if (node)
    {
      node = UsualUnaryConversions (node, (1 == 0));
      type = NodeDataType (node);

      if (!(IsScalarType (type)))
        SyntaxErrorCoord (node->base.coord,
                          "controlling expressions must have scalar type");
    }

  return node;
}

static Node *
SemCheckIf (Node *node, IfNode *u)
{
  u->expr = SemCheckNode (u->expr);
  ((u->expr) ? (void) (0) : __assert_fail ("u->expr", "sem-check.cx", 582, "<func>"));
  u->expr = EnsureScalarType (u->expr);

  if (u->stmt)
    u->stmt = SemCheckNode (u->stmt);
  return node;
}

static Node *
SemCheckIfElse (Node *node, IfElseNode *u)
{
  u->expr = SemCheckNode (u->expr);
  u->expr = UsualUnaryConversions (u->expr, (1 == 0));
  ((u->expr) ? (void) (0) : __assert_fail ("u->expr", "sem-check.cx", 595, "<func>"));
  u->expr = EnsureScalarType (u->expr);

  if (u->iftrue)
    u->iftrue = SemCheckNode (u->iftrue);
  if (u->iffalse)
    u->iffalse = SemCheckNode (u->iffalse);
  return node;
}

static Node *
SemCheckWhile (Node *node, WhileNode *u)
{
  u->expr = SemCheckNode (u->expr);
  u->expr = EnsureScalarType (u->expr);

  u->stmt = SemCheckNode (u->stmt);
  return node;
}

static Node *
SemCheckDo (Node *node, DoNode *u)
{
  u->expr = SemCheckNode (u->expr);
  u->expr = EnsureScalarType (u->expr);

  u->stmt = SemCheckNode (u->stmt);

  return node;
}

static Node *
SemCheckFor (Node *node, ForNode *u)
{
  u->cond = SemCheckNode (u->cond);
  u->cond = EnsureScalarType (u->cond);

  u->init = SemCheckNode (u->init);
  u->next = SemCheckNode (u->next);
  u->stmt = SemCheckNode (u->stmt);

  return node;
}

static Node *
SemCheckGoto (Node *node, GotoNode *u)
{
  return node;
}

static Node *
SemCheckContinue (Node *node, ContinueNode *u)
{
  return node;
}

static Node *
SemCheckBreak (Node *node, BreakNode *u)
{
  return node;
}

static Node *
SemCheckReturn (Node *node, ReturnNode *u)
{
  Node *FunctionRetType = FunctionReturnType (u->proc);

  u->expr = SemCheckNode (u->expr);

  if (u->expr)
    {
      if (IsVoidType (FunctionRetType))
        {
          if (ANSIOnly)
            SyntaxErrorCoord (u->expr->base.coord, "void function cannot return value");
          else if (IsVoidType (NodeDataType (u->expr)))
                       ;

          else
            SyntaxErrorCoord (u->expr->base.coord, "`return ' with a value, in a function returning void");
        }
      else
        u->expr = ReturnConversions (u->expr, FunctionRetType);
    }
  else if (!IsVoidType (FunctionRetType))
    SyntaxErrorCoord (node->base.coord, "non-void function must return value");

  return node;
}

static Node *
SemCheckBlock (Node *node, BlockNode *u)
{

  ListMarker decl_marker;
  Node *decl;

  IterateList (&decl_marker, u->decl);
  while (NextOnList (&decl_marker, (GenericREF) & decl))
    {
      if (decl)
        decl = SemCheckNode (decl);

      if (decl->base.typ == Decl && decl->decl.init == ((void *)0))
        {
          Node *type;

          ((decl->decl.type) ? (void) (0) : __assert_fail ("decl->decl.type", "sem-check.cx", 702, "<func>"));
          type = NodeDataType (decl->decl.type);

          if (NodeIsConstQual (type) && !DeclIsExtern (decl) && !DeclIsTypedef (decl))
            WarningCoord (1, decl->base.coord,
                          "const object should have initialiser: \"%s\"",
                          decl->decl.name);
        }
    }


  u->stmts = SemCheckList (u->stmts);

  if (u->type == ((void *)0))
    {
      Node *item = LastItem (u->stmts);

      ((item) ? (void) (0) : __assert_fail ("item", "sem-check.cx", 719, "<func>"));
      u->type = NodeDataType (item);



    }
  return node;
}
# 736 "sem-check.cx"
static Node *
SemCheckPrim (Node *node, primNode *u)
{
  return node;
}

static Node *
SemCheckTdef (Node *node, tdefNode *u)
{
  return node;
}

static Node *
SemCheckPtr (Node *node, ptrNode *u)
{
  u->type = SemCheckNode (u->type);
  return node;
}

static Node *
SemCheckAdcl (Node *node, adclNode *u)
{
  ARRAYtype *dimp;

  u->type = SemCheckNode (u->type);
  dimp = u->dimp;

  if (dimp->dim)
    {
      Node *dim = SemCheckNode (dimp->dim);
      dimp->dim = dim;

      if (!NodeIsConstant (dim))
        {
          SyntaxErrorCoord (dim->base.coord, "array dimension must be constant");
          dimp->size = 0;
        }
      else if (!IsIntegralType (NodeDataType (dim)))
        {
          SyntaxErrorCoord (dim->base.coord,
                            "array dimension must be an integer type");
          dimp->size = 0;
        }
      else
        {
          int val = NodeConstantIntegralValue (dim);

          ((u->type) ? (void) (0) : __assert_fail ("u->type", "sem-check.cx", 783, "<func>"));


          if (val < 0)
            {
              SyntaxErrorCoord (dim->base.coord, "negative array dimension");
              dimp->size = 0;
            }
          else
            {
              if (val == 0 && ANSIOnly)
                WarningCoord (1, dim->base.coord, "array dimension is zero");

              dimp->size =
                val * NodeSizeof (node, NodeDataType (u->type), (1 == 0));
            }
        }
    }
  else
    dimp->size = 0;

  return node;
}

static Node *
SemCheckFdcl (Node *node, fdclNode *u)
{
  u->args = SemCheckList (u->args);
  u->returns = SemCheckNode (u->returns);
  return node;
}

static Node *
SemCheckSdcl (Node *node, sdclNode *u)
{
  if (tq_has_sue_elaborated (u->tq))
    StructCheckFields (u->type);
  return node;
}

static Node *
SemCheckUdcl (Node *node, udclNode *u)
{
  if (tq_has_sue_elaborated (u->tq))
    UnionCheckFields (u->type);
  return node;
}

static Node *
SemCheckEdcl (Node *node, edclNode *u)
{
  if (tq_has_sue_elaborated (u->tq))
    AssignEnumValues (u->type);
  return node;
}
# 847 "sem-check.cx"
static Node *
SemCheckDecl (Node *node, declNode *u)
{
  u->type = SemCheckNode (u->type);
  u->init = SemCheckNode (u->init);
  u->bitsize = SemCheckNode (u->bitsize);

  if (u->init)
    SemCheckDeclInit (node, tq_has_block_decl (NodeDeclLocation (node)));
  else

  if (DeclIsBlock (node)
      && !DeclIsTypedef (node) && u->type->base.typ != Fdcl)
    NodeSizeof (node, u->type, (1 == 0));

  return node;
}

static Node *
SemCheckAttrib (Node *node, attribNode *u)
{
  return node;
}

static Node *
SemCheckProc (Node *node, procNode *u)
{
  Node *type;
  List *args;

  u->decl = SemCheckNode (u->decl);

  ((u->decl->base.typ == Decl) ? (void) (0) : __assert_fail ("u->decl->base.typ == Decl", "sem-check.cx", 879, "<func>"));
  type = u->decl->decl.type;

  args = type->fdcl.args;


  {
    ListMarker marker;
    Node *item;

    if (!IsVoidArglist (args))
      {
        IterateList (&marker, args);
        while (NextOnList (&marker, (GenericREF) & item))
          if (item->base.typ == Decl)
            {
              if (item->decl.init)
                SyntaxErrorCoord (item->base.coord,
                                  "cannot initialise parameter %s",
                                  item->decl.name);
            }
          else if (IsEllipsis (item))
            ;
          else
            {
              fprintf (stderr, "Unrecognised parameter\n");
              do { GBUF *gb = make_file_gbuf (stderr); PrintNode (gb, item, 0); free_gbuf (gb); } while (0);
              fprintf (stderr, "\n");
              (((1 == 0)) ? (void) (0) : __assert_fail ("(1 == 0)", "sem-check.cx", 907, "<func>"));
            }
      }
  }

  u->body = SemCheckNode (u->body);


  type->fdcl.proc_node = node;

  return node;
}

static Node *
SemCheckText (Node *node, textNode *u)
{
  return node;
}
# 934 "sem-check.cx"
static Node *
SemCheckAsm (Node *node, AsmNode *u)
{
  u->tmplate = SemCheckNode (u->tmplate);
  u->output = SemCheckList (u->output);
  u->input = SemCheckList (u->input);
  u->clobbered = SemCheckList (u->clobbered);






  return node;
}

static Node *
SemCheckAsmArg (Node *node, AsmArgNode *u)
{
  u->constraint = SemCheckNode (u->constraint);
  u->expr = SemCheckNode (u->expr);
  return node;
}
# 965 "sem-check.cx"
static Node *
SemCheckBuiltinVaArg (Node *node, builtinvaargNode *u)
{
  u->expr = SemCheckNode (u->expr);
  u->type = SemCheckNode (u->type);
  return node;
}
# 981 "sem-check.cx"
 Node *
SemCheckNode (Node *node)
{




  if (node == ((void *)0))
    return node;







# 1 "../../include/ast/switch.h" 1




  switch (node->base.typ)
    {
    case Const:
      return SemCheckConst (node, &node->Const);
      break;
    case Id:
      return SemCheckId (node, &node->id);
      break;
    case Binop:
      return SemCheckBinop (node, &node->binop);
      break;
    case Unary:
      return SemCheckUnary (node, &node->unary);
      break;
    case Cast:
      return SemCheckCast (node, &node->cast);
      break;
    case Comma:
      return SemCheckComma (node, &node->comma);
      break;
    case Constructor:
      return SemCheckConstructor (node, &node->constructor);
      break;
    case Ternary:
      return SemCheckTernary (node, &node->ternary);
      break;
    case Array:
      return SemCheckArray (node, &node->array);
      break;
    case Call:
      return SemCheckCall (node, &node->call);
      break;
    case Initialiser:
      return SemCheckInitialiser (node, &node->initialiser);
      break;
    case ImplicitCast:
      return SemCheckImplicitCast (node, &node->implicitcast);
      break;
    case Label:
      return SemCheckLabel (node, &node->label);
      break;
    case Switch:
      return SemCheckSwitch (node, &node->Switch);
      break;
    case Case:
      return SemCheckCase (node, &node->Case);
      break;
    case Default:
      return SemCheckDefault (node, &node->Default);
      break;
    case If:
      return SemCheckIf (node, &node->If);
      break;
    case IfElse:
      return SemCheckIfElse (node, &node->IfElse);
      break;
    case While:
      return SemCheckWhile (node, &node->While);
      break;
    case Do:
      return SemCheckDo (node, &node->Do);
      break;
    case For:
      return SemCheckFor (node, &node->For);
      break;
    case Goto:
      return SemCheckGoto (node, &node->Goto);
      break;
    case Continue:
      return SemCheckContinue (node, &node->Continue);
      break;
    case Break:
      return SemCheckBreak (node, &node->Break);
      break;
    case Return:
      return SemCheckReturn (node, &node->Return);
      break;
    case Block:
      return SemCheckBlock (node, &node->Block);
      break;
    case Prim:
      return SemCheckPrim (node, &node->prim);
      break;
    case Tdef:
      return SemCheckTdef (node, &node->tdef);
      break;
    case Ptr:
      return SemCheckPtr (node, &node->ptr);
      break;
    case Adcl:
      return SemCheckAdcl (node, &node->adcl);
      break;
    case Fdcl:
      return SemCheckFdcl (node, &node->fdcl);
      break;
    case Sdcl:
      return SemCheckSdcl (node, &node->sdcl);
      break;
    case Udcl:
      return SemCheckUdcl (node, &node->udcl);
      break;
    case Edcl:
      return SemCheckEdcl (node, &node->edcl);
      break;

    case Asm:
      return SemCheckAsm (node, &node->Asm);
      break;
    case AsmArg:
      return SemCheckAsmArg (node, &node->AsmArg);
      break;

    case BuiltinVaArg:
      return SemCheckBuiltinVaArg (node, &node->builtinvaarg);
      break;
    case Decl:
      return SemCheckDecl (node, &node->decl);
      break;
    case Attrib:
      return SemCheckAttrib (node, &node->attrib);
      break;
    case Proc:
      return SemCheckProc (node, &node->proc);
      break;
    case Text:
      return SemCheckText (node, &node->text);
      break;
    default:
      Fail ("../../include/ast/switch.h", 133, "unexpected node type");

    }
# 998 "sem-check.cx" 2


                                       Fail ("sem-check.cx", 1000, "UNREACHABLE");
  return ((void *)0);
}

 List *
SemCheckList (List *list)
{
  List *aptr;

  for (aptr = list; aptr; aptr = Rest (aptr))
    {
      Node *item = FirstItem (aptr);

      if (item)
        SetItem (aptr, SemCheckNode (item));
    }

  return list;
}







static void
SemCheckIsArithmeticType (Node *type, Node *op)
{
  ((type) ? (void) (0) : __assert_fail ("type", "sem-check.cx", 1029, "<func>"));
  ((op) ? (void) (0) : __assert_fail ("op", "sem-check.cx", 1030, "<func>"));
  if (!IsArithmeticType (type))
    {
      OpType opcode = (op->base.typ == Unary) ? op->unary.op : op->binop.op;

      SyntaxErrorCoord (op->base.coord,
                        "operand must have arithmetic type: op \"%s\"",
                        Operator[opcode].text);
    }
}

static void
SemCheckIsScalarType (Node *type, Node *op)
{
  if (!IsScalarType (type))
    {
      OpType opcode = (op->base.typ == Unary) ? op->unary.op : op->binop.op;

      SyntaxErrorCoord (op->base.coord,
                        "operand must have scalar type: op \"%s\"",
                        Operator[opcode].text);
    }
}

static void
SemCheckIsIntegralType (Node *type, Node *op)
{
  if (!IsIntegralType (type))
    {
      OpType opcode = (op->base.typ == Unary) ? op->unary.op : op->binop.op;

      SyntaxErrorCoord (op->base.coord,
                        "operand must have integral type: op \"%s\"",
                        Operator[opcode].text);
    }
}

static void
AssignEnumValues (SUEtype *enm)
{
  ListMarker marker;
  Node *c;
  TARGET_SINT current_value = 0;

  ((enm->typ == Edcl) ? (void) (0) : __assert_fail ("enm->typ == Edcl", "sem-check.cx", 1074, "<func>"));
  if (enm->fields == ((void *)0))
    return;

  IterateList (&marker, enm->fields);
  while (NextOnList (&marker, (GenericREF) & c))
    {
      ((c->base.typ == Decl) ? (void) (0) : __assert_fail ("c->base.typ == Decl", "sem-check.cx", 1081, "<func>"));
      if (c->decl.init == ((void *)0))
        {
          c->decl.init = MakeImplicitCast (PrimSint, ((void *)0));
          NodeSetSintValue (c->decl.init, current_value);
        }
      else
        {
          Node *value = SemCheckNode (c->decl.init);
          c->decl.init = value;

          if (!NodeIsConstant (value))
            SyntaxErrorCoord (value->base.coord, "enum initialiser must be constant");
          else if (NodeTypeIsSint (value))
            current_value = NodeConstantSintValue (value);
          else if (NodeTypeIsUint (value))
            {
              current_value = NodeConstantUintValue (value);
              c->decl.init = AssignmentConversions (value, PrimSint);
            }
          else if (NodeTypeIsSlong (value))
            current_value = NodeConstantSlongValue (value);
          else if (NodeTypeIsUlong (value))
            {
              current_value = NodeConstantUlongValue (value);
              c->decl.init = AssignmentConversions (value, PrimSint);
            }
          else
            SyntaxErrorCoord (value->base.coord, "enum initialiser must be type int");
        }

      current_value++;




    }
}

 void
StructCheckFields (SUEtype *sue)
{
  size_t cnt = 0;

  if (sue->visited == (1 == 0))
    {
      int currentbit = 0, max_bitalign = 8;
      ListMarker marker;
      Node *field;


      sue->visited = (1 == 1);


      IterateList (&marker, sue->fields);
      while (NextOnList (&marker, (GenericREF) & field))
        {
          int bitsize, bitalign;
          declNode *decl;
          Node *type;

          ((field->base.typ == Decl) ? (void) (0) : __assert_fail ("field->base.typ == Decl", "sem-check.cx", 1142, "<func>"));
          decl = &(field->decl);
          type = NodeDataType (decl->type);
          ++cnt;

          if (decl->bitsize != ((void *)0))
            {
# 1172 "sem-check.cx"
              if ((type->base.typ != Edcl) &&
                  ((type->base.typ != Prim) || ((type->prim.basic != Schar) &&
                                           (type->prim.basic != Uchar) &&
                                           (type->prim.basic != Sshort) &&
                                           (type->prim.basic != Ushort) &&
                                           (type->prim.basic != Sint) &&
                                           (type->prim.basic != Uint)
                                           )))
                SyntaxErrorCoord (field->base.coord,
                                  "bitfield must be of type "
                                  "enum; or signed or unsigned char, short, int, long");

              decl->bitsize = SemCheckNode (decl->bitsize);
              bitalign = 8;
              if ((!NodeIsConstant (decl->bitsize)) ||
                  (!IsIntegralType (NodeDataType (decl->bitsize))))
                {
                  SyntaxErrorCoord (field->base.coord,
                                    "bitfield size must be a positive "
                                    "integral constant");
                  bitsize = 8;
                }
              else
                bitsize = NodeConstantIntegralValue (decl->bitsize);

              if (bitsize < 0)
                SyntaxErrorCoord (field->base.coord,
                                  "bitfield size must be positive");
              else if (bitsize == 0)
                {
                  if (decl->name == ((void *)0))
                    {

                      if (currentbit % (4 * 8) != 0)
                        currentbit = currentbit + (4 * 8) -
                                     (currentbit % (4 * 8));
                    }
                  else
                    SyntaxErrorCoord (field->base.coord,
                                      "zero width for bit-field %s",
                                      decl->name);
                }
              else
                {
                  if (((currentbit + bitsize - 1) / (4 * 8)) !=
                      (currentbit / (4 * 8)))




                    currentbit =
                      (4 * 8) * ((currentbit / (4 * 8)) + 1);

                  currentbit += bitsize;
                }
            }
          else
            {
              field = SemCheckNode (field);
              SetCurrentOnList (&marker, field);





              bitsize = 8 *
                        NodeSizeof (field, ((void *)0),
                                    (EndOfList (&marker) && cnt > 1));
              bitalign = NodeAlignment (field, ((void *)0)) * 8;


              if (currentbit % bitalign != 0)
                currentbit = currentbit + bitalign -
                             (currentbit % bitalign);

              currentbit += bitsize;
            }

          if (bitalign > max_bitalign)
            max_bitalign = bitalign;
        }


      if (currentbit % max_bitalign != 0)
        currentbit = currentbit + max_bitalign - (currentbit % max_bitalign);

      sue->size = currentbit / 8;
      sue->align = max_bitalign / 8;
    }
}

 void
UnionCheckFields (SUEtype *sue)
{
  if (sue->visited == (1 == 0))
    {
      int max_align = 0, max_size = 0;
      ListMarker marker;
      Node *field;


      sue->visited = (1 == 1);


      IterateList (&marker, sue->fields);
      while (NextOnList (&marker, (GenericREF) & field))
        {
          Node *type;
          int size, align;

          ((field->base.typ == Decl) ? (void) (0) : __assert_fail ("field->base.typ == Decl", "sem-check.cx", 1282, "<func>"));
          type = NodeDataType (field->decl.type);

          if (IsStructType (type) ||
              IsArrayType (type) ||
              IsUnionType (type)
              )
            {
              field = SemCheckNode (field);
              SetCurrentOnList (&marker, field);
            }
          size = NodeSizeof (field, ((void *)0), (1 == 0));
          align = NodeAlignment (field, ((void *)0));







          if (size > max_size)
            max_size = size;
          if (align > max_align)
            max_align = align;
        }

      sue->size = max_size;

      sue->align = max_align;







    }
}

static void
SemCheckCallArgs_Aux (Node *node, List *formals, List *actuals, Bool apply_conversions)
{
  ListMarker fm, am;
  Node *formal, *actual;
  Node *formaltype, *actualtype;
  int formals_len, actuals_len;
  Bool traditional = (formals == ((void *)0));
  Bool variable_length = traditional || IsEllipsis (LastItem (formals));

  formals_len = IsVoidArglist (formals) ? 0 : ListLength (formals);
  actuals_len = ListLength (actuals);

  if (!variable_length && formals_len != actuals_len)
    SyntaxErrorCoord (node->base.coord,
                      "argument mismatch: %d args passed, %d expected",
                      actuals_len, formals_len);
  else if (!IsVoidArglist (formals))
    {
      IterateList (&am, actuals);
      if (!traditional)
        IterateList (&fm, formals);
      while (NextOnList (&am, (GenericREF) & actual))
        {
          if (!traditional)
            {
              NextOnList (&fm, (GenericREF) & formal);
              if (IsEllipsis (formal))
                traditional = (1 == 1);
            }

          if (apply_conversions)
            {
              if (traditional)
                actual = UsualUnaryConversions (actual, (1 == 1));
              else
                {
                  formaltype = NodeDataType (formal);
                  actual = CallConversions (actual, formaltype);
                }
              SetCurrentOnList (&am, actual);
            }
          else
            {




              formaltype = NodeDataType (formal);
              actualtype = NodeDataType (actual);
              if (!TypeEqual (formaltype, actualtype))
                SyntaxErrorCoord (actual->base.coord,
                                  "argument type mismatch in inlet call");
            }
        }
    }
}

static void
SemCheckCallArgs (Node *node, List *formals, List *actuals)
{
  SemCheckCallArgs_Aux (node, formals, actuals, (1 == 1));
}
# 1392 "sem-check.cx"
static Node *
SemCheckAssignment (Node *node, binopNode *u)
{
  Node *left, *right, *ltype, *rtype;
  OpType opcode;

  ((node) ? (void) (0) : __assert_fail ("node", "sem-check.cx", 1398, "<func>"));
  left = u->left;
  right = u->right;
  ((left) ? (void) (0) : __assert_fail ("left", "sem-check.cx", 1401, "<func>"));
  ((right) ? (void) (0) : __assert_fail ("right", "sem-check.cx", 1402, "<func>"));
  ltype = NodeDataType (left);
  rtype = NodeDataType (right);
  ((ltype) ? (void) (0) : __assert_fail ("ltype", "sem-check.cx", 1405, "<func>"));
  ((rtype) ? (void) (0) : __assert_fail ("rtype", "sem-check.cx", 1406, "<func>"));
  opcode = u->op;

  ((left) ? (void) (0) : __assert_fail ("left", "sem-check.cx", 1409, "<func>"));
  ((right) ? (void) (0) : __assert_fail ("right", "sem-check.cx", 1410, "<func>"));
  ((ltype) ? (void) (0) : __assert_fail ("ltype", "sem-check.cx", 1411, "<func>"));
  ((rtype) ? (void) (0) : __assert_fail ("rtype", "sem-check.cx", 1412, "<func>"));


  if (!IsModifiableLvalue (left))
    SyntaxErrorCoord (node->base.coord,
                      "left operand must be modifiable lvalue: op %s",
                      OperatorText (opcode));





  if ((opcode == PLUSassign || opcode == MINUSassign) &&
      IsPointerType (ltype) &&
      IsIntegralType (UsualUnaryConversionType (rtype)))

    NodeSizeof (node, GetDeepBaseType (ltype), (1 == 0));
  else
    u->right = AssignmentConversions (right, ltype);


  u->type = ltype;

  return node;
}

static Node *
SemCheckDot (Node *node, binopNode *u)
{
  Node *field = ((void *)0), *left, *right, *ltype, *rtype, *type;

  ((node) ? (void) (0) : __assert_fail ("node", "sem-check.cx", 1443, "<func>"));

  left = u->left;
  right = u->right;
  ((left) ? (void) (0) : __assert_fail ("left", "sem-check.cx", 1447, "<func>"));
  ((right) ? (void) (0) : __assert_fail ("right", "sem-check.cx", 1448, "<func>"));
  ltype = NodeDataType (left);
  rtype = NodeDataType (right);
  ((ltype) ? (void) (0) : __assert_fail ("ltype", "sem-check.cx", 1451, "<func>"));
  type = NodeDataType (ltype);
  ((type) ? (void) (0) : __assert_fail ("type", "sem-check.cx", 1453, "<func>"));


  if (!(type->base.typ == Sdcl || type->base.typ == Udcl))
    {
      SyntaxErrorCoord (node->base.coord,
                        "left operand of \".\" must be a struct/union object");
      u->type = type;
      return node;
    }
  field = SdclFindField (type, right);


  if (field == ((void *)0))
    {
      SyntaxErrorCoord (node->base.coord,
                        "undefined struct/union member: \"%s\"",
                        rtype->id.text);
      u->type = type;
      return node;
    }
  ((field->decl.type) ? (void) (0) : __assert_fail ("field->decl.type", "sem-check.cx", 1474, "<func>"));
  u->type = NodeDataType (field->decl.type);
  return node;
}

static Node *
SemCheckArrayLabeledInitialiser (Node *node, binopNode *u)
{

  Node *left, *right, *ltype, *rtype, *tmptype;

  ((node) ? (void) (0) : __assert_fail ("node", "sem-check.cx", 1485, "<func>"));

  left = u->left;
  right = u->right;

  ((left) ? (void) (0) : __assert_fail ("left", "sem-check.cx", 1490, "<func>"));
  ((right) ? (void) (0) : __assert_fail ("right", "sem-check.cx", 1491, "<func>"));

  ltype = NodeDataType (left);
  rtype = NodeDataType (right);

  ((ltype) ? (void) (0) : __assert_fail ("ltype", "sem-check.cx", 1496, "<func>"));
  ((rtype) ? (void) (0) : __assert_fail ("rtype", "sem-check.cx", 1497, "<func>"));

  tmptype = NodeDataType (rtype);
  ((tmptype) ? (void) (0) : __assert_fail ("tmptype", "sem-check.cx", 1500, "<func>"));

  u->type = rtype;
  return node;
}

static Node *
SemCheckDesignatedInitialiser (Node *node, binopNode *u)
{
  Node *left, *right, *ltype, *rtype, *tmptype;

  ((node) ? (void) (0) : __assert_fail ("node", "sem-check.cx", 1511, "<func>"));

  left = u->left;
  right = u->right;

  ((left) ? (void) (0) : __assert_fail ("left", "sem-check.cx", 1516, "<func>"));
  ((right) ? (void) (0) : __assert_fail ("right", "sem-check.cx", 1517, "<func>"));

  ltype = NodeDataType (left);
  rtype = NodeDataType (right);

  ((ltype) ? (void) (0) : __assert_fail ("ltype", "sem-check.cx", 1522, "<func>"));
  ((rtype) ? (void) (0) : __assert_fail ("rtype", "sem-check.cx", 1523, "<func>"));

  tmptype = NodeDataType (rtype);
  ((tmptype) ? (void) (0) : __assert_fail ("tmptype", "sem-check.cx", 1526, "<func>"));







  u->type = rtype;
  return node;
}

static Node *
SemCheckArrow (Node *node, binopNode *u)
{
  Node *field = ((void *)0), *right, *left, *ltype, *rtype, *type;

  ((node) ? (void) (0) : __assert_fail ("node", "sem-check.cx", 1543, "<func>"));
  right = u->right;


  left = UsualUnaryConversions (u->left, (1 == 0));

  ((right) ? (void) (0) : __assert_fail ("right", "sem-check.cx", 1549, "<func>"));
  ((left) ? (void) (0) : __assert_fail ("left", "sem-check.cx", 1550, "<func>"));
  ltype = NodeDataType (left);
  rtype = NodeDataType (right);





  ((ltype) ? (void) (0) : __assert_fail ("ltype", "sem-check.cx", 1558, "<func>"));
  ((rtype) ? (void) (0) : __assert_fail ("rtype", "sem-check.cx", 1559, "<func>"));
  type = NodeDataType (ltype);




  ((type) ? (void) (0) : __assert_fail ("type", "sem-check.cx", 1565, "<func>"));


  if (ltype->base.typ != Ptr)
    {
      SyntaxErrorCoord (node->base.coord,
                        "left operand of \"%s\" must be a pointer to a struct/union",
                        OperatorText (u->op));
      u->type = ltype;
      return node;
    }
  ((ltype->ptr.type) ? (void) (0) : __assert_fail ("ltype->ptr.type", "sem-check.cx", 1576, "<func>"));




  type = NodeDataType (ltype->ptr.type);





  if (!(type->base.typ == Sdcl || type->base.typ == Udcl))
    {
      SyntaxErrorCoord (node->base.coord,
                        "left operand of \"%s\" must be a struct/union object",
                        OperatorText (u->op));
      u->type = type;
      return node;
    }

  field = SdclFindField (type, right);


  if (field == ((void *)0))
    {
      SyntaxErrorCoord (node->base.coord,
                        "undefined struct/union member: \"%s\"",
                        rtype->id.text);
      u->type = type;
      return node;
    }
  ((field->decl.type) ? (void) (0) : __assert_fail ("field->decl.type", "sem-check.cx", 1607, "<func>"));




  u->type = MakeMergedType (NodeDataType (field->decl.type),
                            ltype->ptr.type);




  return node;
}

static Node *
SemCheckArithmetic (Node *node, binopNode *u)
{
  Node *left, *right, *ltype, *rtype;
  OpType opcode;

  ((node) ? (void) (0) : __assert_fail ("node", "sem-check.cx", 1627, "<func>"));

  left = UsualUnaryConversions (u->left, (1 == 0)),
  right = UsualUnaryConversions (u->right, (1 == 0)),
  opcode = u->op;

  ((left) ? (void) (0) : __assert_fail ("left", "sem-check.cx", 1633, "<func>"));
  ((right) ? (void) (0) : __assert_fail ("right", "sem-check.cx", 1634, "<func>"));

  switch (opcode)
    {
    case LS:
    case RS:
      break;
    default:
      UsualBinaryConversions (&left, &right);
    }

  ((left) ? (void) (0) : __assert_fail ("left", "sem-check.cx", 1645, "<func>"));
  ((right) ? (void) (0) : __assert_fail ("right", "sem-check.cx", 1646, "<func>"));

  ltype = NodeDataType (left);
  rtype = NodeDataType (right);

  if (ltype == ((void *)0))
    {
      do { GBUF *gb = make_file_gbuf (stdout); PrintNode (gb, left, 0); free_gbuf (gb); } while (0);
      printf ("\n");
    }
  ((left) ? (void) (0) : __assert_fail ("left", "sem-check.cx", 1656, "<func>"));
  ((right) ? (void) (0) : __assert_fail ("right", "sem-check.cx", 1657, "<func>"));
  ((ltype) ? (void) (0) : __assert_fail ("ltype", "sem-check.cx", 1658, "<func>"));
  ((rtype) ? (void) (0) : __assert_fail ("rtype", "sem-check.cx", 1659, "<func>"));

  switch (opcode)
    {
    case '+':

      if (IsIntegralType (ltype) && IsPointerType (rtype))
        {
          Node *tnode = left, *ttype = ltype;

          left = right;
          ltype = rtype;
          right = tnode;
          rtype = ttype;
        }
      if (!((IsArithmeticType (ltype) && IsArithmeticType (rtype)) ||
            (IsPointerType (ltype) && IsIntegralType (rtype))))
        SyntaxErrorCoord (node->base.coord,
                          "operands must have arithmetic type or ptr/int: op \"+\"");
      u->type = ltype;

      if (IsPointerType (ltype))



        (void)NodeSizeof (left, GetShallowBaseType (ltype), (1 == 0));
      if (NodeIsConstant (left) && NodeIsConstant (right))
        {
          if (NodeTypeIsSint (left))
            {
              int lval = NodeConstantSintValue (left), rval = NodeConstantSintValue (right);

              NodeSetSintValue (node, lval + rval);
            }
          else if (NodeTypeIsUint (left))
            {
              unsigned int lval = NodeConstantUintValue (left),
                           rval = NodeConstantUintValue (right);

              NodeSetUintValue (node, lval + rval);
            }
          else if (NodeTypeIsSlong (left))
            {
              long lval = NodeConstantSlongValue (left), rval = NodeConstantSlongValue (right);

              NodeSetSlongValue (node, lval + rval);
            }
          else if (NodeTypeIsUlong (left))
            {
              unsigned long lval = NodeConstantUlongValue (left),
                            rval = NodeConstantUlongValue (right);

              NodeSetUlongValue (node, lval + rval);
            }
          else if (NodeTypeIsFloat (left))
            {
              float lval = NodeConstantFloatValue (left), rval = NodeConstantFloatValue (right);

              NodeSetFloatValue (node, lval + rval);
            }
          else if (NodeTypeIsDouble (left))
            {
              double lval = NodeConstantDoubleValue (left),
                     rval = NodeConstantDoubleValue (right);

              NodeSetDoubleValue (node, lval + rval);
            }
        }
      break;

    case '-':
      if (!((IsArithmeticType (ltype) && IsArithmeticType (rtype)) ||
            (IsPointerType (ltype) && IsIntegralType (rtype)) ||
            (IsPointerType (ltype) && IsPointerType (rtype))))
        SyntaxErrorCoord (node->base.coord,
                          "operands have incompatible types: op \"-\"");
      if (ltype->base.typ == Ptr && rtype->base.typ == Ptr)
        u->type = PrimSint;
      else
        u->type = ltype;

      if (NodeIsConstant (left) && NodeIsConstant (right))
        {
          if (NodeTypeIsSint (left))
            {
              int lval = NodeConstantSintValue (left), rval = NodeConstantSintValue (right);

              NodeSetSintValue (node, lval - rval);
            }
          else if (NodeTypeIsUint (left))
            {
              unsigned int lval = NodeConstantUintValue (left),
                           rval = NodeConstantUintValue (right);

              NodeSetUintValue (node, lval - rval);
            }
          else if (NodeTypeIsSlong (left))
            {
              long lval = NodeConstantSlongValue (left), rval = NodeConstantSlongValue (right);

              NodeSetSlongValue (node, lval - rval);
            }
          else if (NodeTypeIsUlong (left))
            {
              unsigned long lval = NodeConstantUlongValue (left),
                            rval = NodeConstantUlongValue (right);

              NodeSetUlongValue (node, lval - rval);
            }
          else if (NodeTypeIsFloat (left))
            {
              float lval = NodeConstantFloatValue (left), rval = NodeConstantFloatValue (right);

              NodeSetFloatValue (node, lval - rval);
            }
          else if (NodeTypeIsDouble (left))
            {
              double lval = NodeConstantDoubleValue (left),
                     rval = NodeConstantDoubleValue (right);

              NodeSetDoubleValue (node, lval - rval);
            }
        }
      break;

    case '*':
      if (!(IsArithmeticType (ltype) && IsArithmeticType (rtype)))
        SyntaxErrorCoord (node->base.coord,
                          "operands must have arithmetic type: op \"%s\"",
                          Operator[opcode].text);

      u->type = ltype;

      if (NodeIsConstant (left) && NodeIsConstant (right))
        {
          if (NodeTypeIsSint (left))
            {
              int lval = NodeConstantSintValue (left), rval = NodeConstantSintValue (right);

              NodeSetSintValue (node, lval * rval);
            }
          else if (NodeTypeIsUint (left))
            {
              unsigned int lval = NodeConstantUintValue (left),
                           rval = NodeConstantUintValue (right);

              NodeSetUintValue (node, lval * rval);
            }
          else if (NodeTypeIsSlong (left))
            {
              long lval = NodeConstantSlongValue (left), rval = NodeConstantSlongValue (right);

              NodeSetSlongValue (node, lval * rval);
            }
          else if (NodeTypeIsUlong (left))
            {
              unsigned long lval = NodeConstantUlongValue (left),
                            rval = NodeConstantUlongValue (right);

              NodeSetUlongValue (node, lval * rval);
            }
          else if (NodeTypeIsFloat (left))
            {
              float lval = NodeConstantFloatValue (left), rval = NodeConstantFloatValue (right);

              NodeSetFloatValue (node, lval * rval);
            }
          else if (NodeTypeIsDouble (left))
            {
              double lval = NodeConstantDoubleValue (left),
                     rval = NodeConstantDoubleValue (right);

              NodeSetDoubleValue (node, lval * rval);
            }
        }
      break;

    case '/':
      if (!(IsArithmeticType (ltype) && IsArithmeticType (rtype)))
        SyntaxErrorCoord (node->base.coord,
                          "operands must have arithmetic type: op \"%s\"",
                          Operator[opcode].text);

      if (NodeIsConstant (left) && NodeIsConstant (right))
        {
          if (NodeTypeIsSint (left))
            {
              int lval = NodeConstantSintValue (left), rval = NodeConstantSintValue (right);

              if (rval == 0)
                SyntaxErrorCoord (node->base.coord,
                                  "attempt to divide constant by 0");
              else
                NodeSetSintValue (node, lval / rval);
            }
          else if (NodeTypeIsUint (left))
            {
              unsigned int lval = NodeConstantUintValue (left),
                           rval = NodeConstantUintValue (right);

              if (rval == 0)
                SyntaxErrorCoord (node->base.coord,
                                  "attempt to divide constant by 0");
              else
                NodeSetUintValue (node, lval / rval);
            }
          else if (NodeTypeIsSlong (left))
            {
              long lval = NodeConstantSlongValue (left), rval = NodeConstantSlongValue (right);

              if (rval == 0)
                SyntaxErrorCoord (node->base.coord,
                                  "attempt to divide constant by 0");
              else
                NodeSetSlongValue (node, lval / rval);
            }
          else if (NodeTypeIsUlong (left))
            {
              unsigned long lval = NodeConstantUlongValue (left),
                            rval = NodeConstantUlongValue (right);

              if (rval == 0)
                SyntaxErrorCoord (node->base.coord,
                                  "attempt to divide constant by 0");
              else
                NodeSetUlongValue (node, lval / rval);
            }
          else if (NodeTypeIsFloat (left))
            {
              float lval = NodeConstantFloatValue (left), rval = NodeConstantFloatValue (right);

              if (rval == 0)
                SyntaxErrorCoord (node->base.coord,
                                  "attempt to divide constant by 0");
              else
                NodeSetFloatValue (node, lval / rval);
            }
          else if (NodeTypeIsDouble (left))
            {
              double lval = NodeConstantDoubleValue (left),
                     rval = NodeConstantDoubleValue (right);

              if (rval == 0)
                SyntaxErrorCoord (node->base.coord,
                                  "attempt to divide constant by 0");
              else
                NodeSetDoubleValue (node, lval / rval);
            }
        }
      u->type = ltype;
      break;

    case '%':
      if (!(IsIntegralType (ltype) && IsIntegralType (rtype)))
        SyntaxErrorCoord (node->base.coord,
                          "operands must have integral type: op \"%s\"",
                          Operator[opcode].text);
      if (NodeIsConstant (left) && NodeIsConstant (right))
        {
          if (NodeTypeIsSint (left))
            {
              int lval = NodeConstantSintValue (left), rval = NodeConstantSintValue (right);

              NodeSetSintValue (node, lval % rval);
            }
          else if (NodeTypeIsUint (left))
            {
              unsigned int lval = NodeConstantUintValue (left),
                           rval = NodeConstantUintValue (right);

              NodeSetUintValue (node, lval % rval);
            }
          else if (NodeTypeIsSlong (left))
            {
              long lval = NodeConstantSlongValue (left), rval = NodeConstantSlongValue (right);

              NodeSetSlongValue (node, lval % rval);
            }
          else if (NodeTypeIsUlong (left))
            {
              unsigned long lval = NodeConstantUlongValue (left),
                            rval = NodeConstantUlongValue (right);

              NodeSetUlongValue (node, lval % rval);
            }
        }
      u->type = ltype;
      break;
    case LS:
      if (!(IsIntegralType (ltype) && IsIntegralType (rtype)))
        SyntaxErrorCoord (node->base.coord,
                          "operands must have integral type: op \"%s\"",
                          Operator[opcode].text);
      if (NodeIsConstant (left) && NodeIsConstant (right))
        {
          unsigned long rval = NodeConstantIntegralValue (right);

          if (NodeTypeIsSint (left))
            {
              int lval = NodeConstantSintValue (left);

              NodeSetSintValue (node, lval << rval);
            }
          else if (NodeTypeIsUint (left))
            {
              unsigned int lval = NodeConstantUintValue (left);

              NodeSetUintValue (node, lval << rval);
            }
          else if (NodeTypeIsSlong (left))
            {
              long lval = NodeConstantSlongValue (left);

              NodeSetSlongValue (node, lval << rval);
            }
          else if (NodeTypeIsUlong (left))
            {
              unsigned long lval = NodeConstantUlongValue (left);

              NodeSetUlongValue (node, lval << rval);
            }
        }
      u->type = ltype;
      break;
    case RS:
      if (!(IsIntegralType (ltype) && IsIntegralType (rtype)))
        SyntaxErrorCoord (node->base.coord,
                          "operands must have integral type: op \"%s\"",
                          Operator[opcode].text);
      if (NodeIsConstant (left) && NodeIsConstant (right))
        {
          unsigned long rval = NodeConstantIntegralValue (right);

          if (NodeTypeIsSint (left))
            {
              int lval = NodeConstantSintValue (left);

              NodeSetSintValue (node, lval >> rval);
            }
          else if (NodeTypeIsUint (left))
            {
              unsigned int lval = NodeConstantUintValue (left);

              NodeSetUintValue (node, lval >> rval);
            }
          else if (NodeTypeIsSlong (left))
            {
              long lval = NodeConstantSlongValue (left);

              NodeSetSlongValue (node, lval >> rval);
            }
          else if (NodeTypeIsUlong (left))
            {
              unsigned long lval = NodeConstantUlongValue (left);

              NodeSetUlongValue (node, lval >> rval);
            }
        }
      u->type = ltype;
      break;
    case '&':
      if (!(IsIntegralType (ltype) && IsIntegralType (rtype)))
        SyntaxErrorCoord (node->base.coord,
                          "operands must have integral type: op \"%s\"",
                          Operator[opcode].text);
      if (NodeIsConstant (left) && NodeIsConstant (right))
        {
          if (NodeTypeIsSint (left))
            {
              int lval = NodeConstantSintValue (left), rval = NodeConstantSintValue (right);

              NodeSetSintValue (node, lval & rval);
            }
          else if (NodeTypeIsUint (left))
            {
              unsigned int lval = NodeConstantUintValue (left),
                           rval = NodeConstantUintValue (right);

              NodeSetUintValue (node, lval & rval);
            }
          else if (NodeTypeIsSlong (left))
            {
              long lval = NodeConstantSlongValue (left), rval = NodeConstantSlongValue (right);

              NodeSetSlongValue (node, lval & rval);
            }
          else if (NodeTypeIsUlong (left))
            {
              unsigned long lval = NodeConstantUlongValue (left),
                            rval = NodeConstantUlongValue (right);

              NodeSetUlongValue (node, lval & rval);
            }
        }
      u->type = ltype;
      break;
    case '^':
      if (!(IsIntegralType (ltype) && IsIntegralType (rtype)))
        SyntaxErrorCoord (node->base.coord,
                          "operands must have integral type: op \"%s\"",
                          Operator[opcode].text);
      if (NodeIsConstant (left) && NodeIsConstant (right))
        {
          if (NodeTypeIsSint (left))
            {
              int lval = NodeConstantSintValue (left), rval = NodeConstantSintValue (right);

              NodeSetSintValue (node, lval ^ rval);
            }
          else if (NodeTypeIsUint (left))
            {
              unsigned int lval = NodeConstantUintValue (left),
                           rval = NodeConstantUintValue (right);

              NodeSetUintValue (node, lval ^ rval);
            }
          else if (NodeTypeIsSlong (left))
            {
              long lval = NodeConstantSlongValue (left), rval = NodeConstantSlongValue (right);

              NodeSetSlongValue (node, lval ^ rval);
            }
          else if (NodeTypeIsUlong (left))
            {
              unsigned long lval = NodeConstantUlongValue (left),
                            rval = NodeConstantUlongValue (right);

              NodeSetUlongValue (node, lval ^ rval);
            }
        }
      u->type = ltype;
      break;
    case '|':
      if (!(IsIntegralType (ltype) && IsIntegralType (rtype)))
        SyntaxErrorCoord (node->base.coord,
                          "operands must have integral type: op \"%s\"",
                          Operator[opcode].text);
      if (NodeIsConstant (left) && NodeIsConstant (right))
        {
          if (NodeTypeIsSint (left))
            {
              int lval = NodeConstantSintValue (left), rval = NodeConstantSintValue (right);

              NodeSetSintValue (node, lval | rval);
            }
          else if (NodeTypeIsUint (left))
            {
              unsigned int lval = NodeConstantUintValue (left),
                           rval = NodeConstantUintValue (right);

              NodeSetUintValue (node, lval | rval);
            }
          else if (NodeTypeIsSlong (left))
            {
              long lval = NodeConstantSlongValue (left), rval = NodeConstantSlongValue (right);

              NodeSetSlongValue (node, lval | rval);
            }
          else if (NodeTypeIsUlong (left))
            {
              unsigned long lval = NodeConstantUlongValue (left),
                            rval = NodeConstantUlongValue (right);

              NodeSetUlongValue (node, lval | rval);
            }
        }
      u->type = ltype;
      break;

    case ANDAND:
      if (NodeIsConstant (left) && NodeIsConstant (right))
        {
          int lval = IsConstantZero (left), rval = IsConstantZero (right);

          NodeSetSintValue (node, !lval && !rval);
        }
      u->type = PrimSint;
      break;
    case OROR:
      if (NodeIsConstant (left) && NodeIsConstant (right))
        {
          int lval = IsConstantZero (left), rval = IsConstantZero (right);

          NodeSetSintValue (node, !lval || !rval);
        }
      u->type = PrimSint;
      break;
    default:
      fprintf (stderr, "Internal Error! Unrecognised arithmetic operator\n");
      (((1 == 0)) ? (void) (0) : __assert_fail ("(1 == 0)", "sem-check.cx", 2148, "<func>"));
    }

  u->left = left;
  u->right = right;
  return node;
}

static Node *
SemCheckComparison (Node *node, binopNode *u)
{
  Node *left = UsualUnaryConversions (u->left, (1 == 0)), *right = UsualUnaryConversions (u->right, (1 == 0)),
  *ltype, *rtype;
  OpType opcode = u->op;

  ((left) ? (void) (0) : __assert_fail ("left", "sem-check.cx", 2163, "<func>"));
  ((right) ? (void) (0) : __assert_fail ("right", "sem-check.cx", 2164, "<func>"));

  UsualBinaryConversions (&left, &right);

  ((left) ? (void) (0) : __assert_fail ("left", "sem-check.cx", 2168, "<func>"));
  ((right) ? (void) (0) : __assert_fail ("right", "sem-check.cx", 2169, "<func>"));

  ltype = NodeDataType (left);
  rtype = NodeDataType (right);

  ((ltype) ? (void) (0) : __assert_fail ("ltype", "sem-check.cx", 2174, "<func>"));
  ((rtype) ? (void) (0) : __assert_fail ("rtype", "sem-check.cx", 2175, "<func>"));

  switch (opcode)
    {
    case '<':
      u->type = PrimSint;

      if (IsArithmeticType (ltype) && IsArithmeticType (rtype))
        {
          if (NodeIsConstant (left) && NodeIsConstant (right))
            {
              if (NodeTypeIsSint (left))
                {
                  int lval = NodeConstantSintValue (left),
                      rval = NodeConstantSintValue (right);

                  NodeSetSintValue (node, lval < rval);
                }
              else if (NodeTypeIsUint (left))
                {
                  unsigned int lval = NodeConstantUintValue (left),
                               rval = NodeConstantUintValue (right);

                  NodeSetSintValue (node, lval < rval);
                }
              else if (NodeTypeIsSlong (left))
                {
                  long lval = NodeConstantSlongValue (left),
                       rval = NodeConstantSlongValue (right);

                  NodeSetSintValue (node, lval < rval);
                }
              else if (NodeTypeIsUlong (left))
                {
                  unsigned long lval = NodeConstantUlongValue (left),
                                rval = NodeConstantUlongValue (right);

                  NodeSetSintValue (node, lval < rval);
                }
              else if (NodeTypeIsFloat (left))
                {
                  float lval = NodeConstantFloatValue (left),
                        rval = NodeConstantFloatValue (right);

                  NodeSetSintValue (node, lval < rval);
                }
              else if (NodeTypeIsDouble (left))
                {
                  double lval = NodeConstantDoubleValue (left),
                         rval = NodeConstantDoubleValue (right);

                  NodeSetSintValue (node, lval < rval);
                }
            }
        }
      else if (IsPointerType (ltype) && IsPointerType (rtype))
        UsualPointerConversions (&left, &right, (1 == 0));
      else
        SyntaxErrorCoord (node->base.coord,
                          "operands have incompatible types: op \"%s\"",
                          OperatorText (opcode));
      break;
    case LE:
      u->type = PrimSint;

      if (IsArithmeticType (ltype) && IsArithmeticType (rtype))
        {
          if (NodeIsConstant (left) && NodeIsConstant (right))
            {
              if (NodeTypeIsSint (left))
                {
                  int lval = NodeConstantSintValue (left),
                      rval = NodeConstantSintValue (right);

                  NodeSetSintValue (node, lval <= rval);
                }
              else if (NodeTypeIsUint (left))
                {
                  unsigned int lval = NodeConstantUintValue (left),
                               rval = NodeConstantUintValue (right);

                  NodeSetSintValue (node, lval <= rval);
                }
              else if (NodeTypeIsSlong (left))
                {
                  long lval = NodeConstantSlongValue (left),
                       rval = NodeConstantSlongValue (right);

                  NodeSetSintValue (node, lval <= rval);
                }
              else if (NodeTypeIsUlong (left))
                {
                  unsigned long lval = NodeConstantUlongValue (left),
                                rval = NodeConstantUlongValue (right);

                  NodeSetSintValue (node, lval <= rval);
                }
              else if (NodeTypeIsFloat (left))
                {
                  float lval = NodeConstantFloatValue (left),
                        rval = NodeConstantFloatValue (right);

                  NodeSetSintValue (node, lval <= rval);
                }
              else if (NodeTypeIsDouble (left))
                {
                  double lval = NodeConstantDoubleValue (left),
                         rval = NodeConstantDoubleValue (right);

                  NodeSetSintValue (node, lval <= rval);
                }
            }
        }
      else if (IsPointerType (ltype) && IsPointerType (rtype))
        UsualPointerConversions (&left, &right, (1 == 0));
      else
        SyntaxErrorCoord (node->base.coord,
                          "operands have incompatible types: op \"%s\"",
                          OperatorText (opcode));
      break;
    case '>':
      u->type = PrimSint;

      if (IsArithmeticType (ltype) && IsArithmeticType (rtype))
        {
          if (NodeIsConstant (left) && NodeIsConstant (right))
            {
              if (NodeTypeIsSint (left))
                {
                  int lval = NodeConstantSintValue (left),
                      rval = NodeConstantSintValue (right);

                  NodeSetSintValue (node, lval > rval);
                }
              else if (NodeTypeIsUint (left))
                {
                  unsigned int lval = NodeConstantUintValue (left),
                               rval = NodeConstantUintValue (right);

                  NodeSetSintValue (node, lval > rval);
                }
              else if (NodeTypeIsSlong (left))
                {
                  long lval = NodeConstantSlongValue (left),
                       rval = NodeConstantSlongValue (right);

                  NodeSetSintValue (node, lval > rval);
                }
              else if (NodeTypeIsUlong (left))
                {
                  unsigned long lval = NodeConstantUlongValue (left),
                                rval = NodeConstantUlongValue (right);

                  NodeSetSintValue (node, lval > rval);
                }
              else if (NodeTypeIsFloat (left))
                {
                  float lval = NodeConstantFloatValue (left),
                        rval = NodeConstantFloatValue (right);

                  NodeSetSintValue (node, lval > rval);
                }
              else if (NodeTypeIsDouble (left))
                {
                  double lval = NodeConstantDoubleValue (left),
                         rval = NodeConstantDoubleValue (right);

                  NodeSetSintValue (node, lval > rval);
                }
            }
        }
      else if (IsPointerType (ltype) && IsPointerType (rtype))
        UsualPointerConversions (&left, &right, (1 == 0));
      else
        SyntaxErrorCoord (node->base.coord,
                          "operands have incompatible types: op \"%s\"",
                          OperatorText (opcode));
      break;
    case GE:
      u->type = PrimSint;

      if (IsArithmeticType (ltype) && IsArithmeticType (rtype))
        {
          if (NodeIsConstant (left) && NodeIsConstant (right))
            {
              if (NodeTypeIsSint (left))
                {
                  int lval = NodeConstantSintValue (left),
                      rval = NodeConstantSintValue (right);

                  NodeSetSintValue (node, lval >= rval);
                }
              else if (NodeTypeIsUint (left))
                {
                  unsigned int lval = NodeConstantUintValue (left),
                               rval = NodeConstantUintValue (right);

                  NodeSetSintValue (node, lval >= rval);
                }
              else if (NodeTypeIsSlong (left))
                {
                  long lval = NodeConstantSlongValue (left),
                       rval = NodeConstantSlongValue (right);

                  NodeSetSintValue (node, lval >= rval);
                }
              else if (NodeTypeIsUlong (left))
                {
                  unsigned long lval = NodeConstantUlongValue (left),
                                rval = NodeConstantUlongValue (right);

                  NodeSetSintValue (node, lval >= rval);
                }
              else if (NodeTypeIsFloat (left))
                {
                  float lval = NodeConstantFloatValue (left),
                        rval = NodeConstantFloatValue (right);

                  NodeSetSintValue (node, lval >= rval);
                }
              else if (NodeTypeIsDouble (left))
                {
                  double lval = NodeConstantDoubleValue (left),
                         rval = NodeConstantDoubleValue (right);

                  NodeSetSintValue (node, lval >= rval);
                }
            }
        }
      else if (IsPointerType (ltype) && IsPointerType (rtype))
        UsualPointerConversions (&left, &right, (1 == 0));
      else
        SyntaxErrorCoord (node->base.coord,
                          "operands have incompatible types: op \"%s\"",
                          OperatorText (opcode));
      break;

    case EQ:
      u->type = PrimSint;

      if (IsArithmeticType (ltype) && IsArithmeticType (rtype))
        {
          if (NodeIsConstant (left) && NodeIsConstant (right))
            {
              if (NodeTypeIsSint (left))
                {
                  int lval = NodeConstantSintValue (left),
                      rval = NodeConstantSintValue (right);

                  NodeSetSintValue (node, lval == rval);
                }
              else if (NodeTypeIsUint (left))
                {
                  unsigned int lval = NodeConstantUintValue (left),
                               rval = NodeConstantUintValue (right);

                  NodeSetSintValue (node, lval == rval);
                }
              else if (NodeTypeIsSlong (left))
                {
                  long lval = NodeConstantSlongValue (left),
                       rval = NodeConstantSlongValue (right);

                  NodeSetSintValue (node, lval == rval);
                }
              else if (NodeTypeIsUlong (left))
                {
                  unsigned long lval = NodeConstantUlongValue (left),
                                rval = NodeConstantUlongValue (right);

                  NodeSetSintValue (node, lval == rval);
                }
              else if (NodeTypeIsFloat (left))
                {
                  float lval = NodeConstantFloatValue (left),
                        rval = NodeConstantFloatValue (right);

                  NodeSetSintValue (node, lval == rval);
                }
              else if (NodeTypeIsDouble (left))
                {
                  double lval = NodeConstantDoubleValue (left),
                         rval = NodeConstantDoubleValue (right);

                  NodeSetSintValue (node, lval == rval);
                }
            }
        }
      else if (IsPointerType (ltype) && IsPointerType (rtype))
        UsualPointerConversions (&left, &right, (1 == 1));
      else if ((IsPointerType (ltype) && IsConstantZero (right)) ||
               (IsConstantZero (left) && IsPointerType (rtype)))
        ;
      else
        SyntaxErrorCoord (node->base.coord,
                          "operands have incompatible types: op \"%s\"",
                          OperatorText (opcode));
      break;
    case NE:
      u->type = PrimSint;

      if (IsArithmeticType (ltype) && IsArithmeticType (rtype))
        {
          if (NodeIsConstant (left) && NodeIsConstant (right))
            {
              if (NodeTypeIsSint (left))
                {
                  int lval = NodeConstantSintValue (left),
                      rval = NodeConstantSintValue (right);

                  NodeSetSintValue (node, lval != rval);
                }
              else if (NodeTypeIsUint (left))
                {
                  unsigned int lval = NodeConstantUintValue (left),
                               rval = NodeConstantUintValue (right);

                  NodeSetSintValue (node, lval != rval);
                }
              else if (NodeTypeIsSlong (left))
                {
                  long lval = NodeConstantSlongValue (left),
                       rval = NodeConstantSlongValue (right);

                  NodeSetSintValue (node, lval != rval);
                }
              else if (NodeTypeIsUlong (left))
                {
                  unsigned long lval = NodeConstantUlongValue (left),
                                rval = NodeConstantUlongValue (right);

                  NodeSetSintValue (node, lval != rval);
                }
              else if (NodeTypeIsFloat (left))
                {
                  float lval = NodeConstantFloatValue (left),
                        rval = NodeConstantFloatValue (right);

                  NodeSetSintValue (node, lval != rval);
                }
              else if (NodeTypeIsDouble (left))
                {
                  double lval = NodeConstantDoubleValue (left),
                         rval = NodeConstantDoubleValue (right);

                  NodeSetSintValue (node, lval != rval);
                }
            }
        }
      else if (IsPointerType (ltype) && IsPointerType (rtype))
        UsualPointerConversions (&left, &right, (1 == 1));
      else if ((IsPointerType (ltype) && IsConstantZero (right)) ||
               (IsConstantZero (left) && IsPointerType (rtype)))
        ;
      else
        SyntaxErrorCoord (node->base.coord,
                          "operands have incompatible types: op \"%s\"",
                          OperatorText (opcode));
      break;
    default:
      fprintf (stdout, "Internal Error: Unrecognised comparison operator\n");
      (((1 == 0)) ? (void) (0) : __assert_fail ("(1 == 0)", "sem-check.cx", 2536, "<func>"));
    }

  u->left = left;
  u->right = right;
  return node;
}







struct SwitchCheck
{
  Node *defaultcase;
  unsigned tablesize;
  Node *table[1];

};



static struct SwitchCheck *
NewSwitchCheck (List *cases)
{
  int i;
  int count = ListLength (cases);
  int tablesize = count * 2;
  struct SwitchCheck *check = calloc (sizeof (struct SwitchCheck) + sizeof (Node *) * (tablesize - 1), 1);

  ((check) ? (void) (0) : __assert_fail ("check", "sem-check.cx", 2568, "<func>"));

  check->defaultcase = ((void *)0);
  check->tablesize = tablesize;
  for (i = 0; i < tablesize; ++i)
    check->table[i] = ((void *)0);

  return check;
}

static void
FreeSwitchCheck (struct SwitchCheck *check)
{

  check->tablesize = 0;
  free (check);
}

static void
SwitchCheckAddCase (struct SwitchCheck *check, Node *expr)
{
  unsigned long val;
  unsigned h;

  ((check) ? (void) (0) : __assert_fail ("check", "sem-check.cx", 2592, "<func>"));
  ((expr) ? (void) (0) : __assert_fail ("expr", "sem-check.cx", 2593, "<func>"));

  val = NodeConstantIntegralValue (expr);





  for (h = val % check->tablesize;
       check->table[h] != ((void *)0);
       h = (h + 1) % check->tablesize)
    if (NodeConstantIntegralValue (check->table[h]) == val)
      {
        SyntaxErrorCoord (expr->base.coord, "duplicate case label");
        fprintf (stderr, "\tOriginal case: ");
        { if (PrintLineOffset) fprintf (stderr, "%s:%d:%d", FileNames[(check->table[h]->base.coord).file], (int)(check->table[h]->base.coord).line, (int)(check->table[h]->base.coord).offset); else fprintf (stderr, "%s:%d", FileNames[(check->table[h]->base.coord).file], (int)(check->table[h]->base.coord).line); };
        fputc ('\n', stderr);
        return;
      }
  check->table[h] = expr;
}

static void
SwitchCheckAddDefault (struct SwitchCheck *check, Node *node)
{
  ((check) ? (void) (0) : __assert_fail ("check", "sem-check.cx", 2618, "<func>"));
  ((node) ? (void) (0) : __assert_fail ("node", "sem-check.cx", 2619, "<func>"));

  if (check->defaultcase != ((void *)0))
    SyntaxErrorCoord (node->base.coord, "multiple default cases");
  else
    check->defaultcase = node;
}
