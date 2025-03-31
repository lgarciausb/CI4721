type B := {b : int, c : float}

int main(){
  B y  := new B(10,5.4);
  int z0 := 8 + 9; 
  for({b : int, c : float} : y){
    b + c;
    break;
    return 5;
  };
}

