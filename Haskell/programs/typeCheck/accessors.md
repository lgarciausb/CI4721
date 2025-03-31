type A := {a : int}
type B := {a : int}
type C := {a : char}

int main(){
    A x := new A(1);
    B y := new B(2);
    C z := new C("a");

    int xa := x.a;
    int ya := y.a;

    /* Sólo debe fallar aquí ya que este elemento es de tipo char y no int. */
    int za := z.a;
}