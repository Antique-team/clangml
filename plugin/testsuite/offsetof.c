
struct astruct {
 int myarray[20];
};
int i = __builtin_offsetof (struct astruct, myarray[0]);
