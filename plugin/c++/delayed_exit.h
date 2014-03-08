#ifndef DELAYED_EXIT_H
#define DELAYED_EXIT_H

struct delayed_exit
{
  int code;
  ~delayed_exit ();
};

#endif /* DELAYED_EXIT_H */
