void foo (int opcode)
{
  void *p[] = {
    &&push,
    &&pop,
    &&add,
  };

  goto *p[opcode];

push:
  // code for push
  goto end;
pop:
  // code for pop
  goto end;
add:
  // code for add
  goto end;

end:
  return;
}
