int fibo(int n){
  match n with
    0 => {return n;}
    1 => {return n;}
    _ => {return fibo(n-1) + fibo(n-2);}
  ;

}

int main(){
  print(to_string(fibo(to_int(input("Input a number:\n> ")))));
  return 0;
}

