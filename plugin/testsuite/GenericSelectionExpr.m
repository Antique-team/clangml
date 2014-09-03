
void f() {
  int i =  _Generic(1.0 , double: 1, float: 2, default: 3);
  int j =  _Generic(1.0f, double: 1, float: 2, default: 3);
  int k =  _Generic(1   , double: 1, float: 2, default: 3);
}
