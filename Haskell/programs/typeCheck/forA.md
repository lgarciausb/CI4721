type A := {a : int}
type B := {b : int, c : float}
type C := {d : A, e : B}

int main(){
  A x  := new A(1);
  B y  := new B(10,5.4);
  C z  := new C(x,y);
  int z0 := 8 + 9; 
  for({d : A, e : B} : z){
    x := d;
    y := e;
    break;
    return 5;
  };
}

