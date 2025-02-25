int sum(int i){
  int provided := to_int(input("Input a number. 0 to exit.\n> "));
  match provided with
    0 => {return i;}
    _ => {return sum(i + provided);}
  ;
}

int main(){
  print(to_string(sum(0)));
  return 0;
}

