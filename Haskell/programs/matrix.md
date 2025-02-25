int vector_multiplication(vector<int> ms, vector<int> ns){
  int i := 0;
  for (m : ms){
    for(n : ns){
      i := m + n;
    };
  };

  return i;
}

unit matrix_multiplication(vector<vector<int>> mss, vector<vector<int>> nss, vector<vector<int>> res by reference){

  vector<int> acc := new vector<int>();
  for (ms : mss){
    for(ns : nss){
      append(acc,vector_multiplication(ms,ns)); 
    };
    append(res,acc);
    for(_ : nss){pop(acc);};
  };
  return unit;

}

int main(){
  vector<vector<int>> mss := [[1,2,3],[4,5,6]];
  vector<vector<int>> nss := [[7,8],[9,10],[11,12]];
  vector<vector<int>> res := new vector<vector<int>>();
  matrix_multiplication(mss,nss,res);
  return 0;
}


