#include <stdio.h>
#include <caml/mlvalues.h>

CAMLprim value
caml_print_hello(value unit)
{
    printf("Hello from C\n");
    return Val_unit;
}

#include <caml/mlvalues.h>
#include <caml/callback.h>

void hello_closure()
{
    static value * closure_f = NULL;
    if (closure_f == NULL) {
        closure_f = caml_named_value("Hello callback");
    }
    caml_callback(*closure_f, Val_unit);
}

int main(int argc, char **argv)
{
    caml_main(argv);
    hello_closure();
    return 0;
}