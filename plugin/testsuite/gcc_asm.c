
int main (int argc, char** argv) {
    int a;
    asm("some asm" : "=a" (a) : "a" (a) : "12");
    asm("some asm");
}
