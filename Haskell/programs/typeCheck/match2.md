int main(){
    /* No debe fallar con el break/continue ya que son keywords/acciones pertenecientes al for. */
    vector<string> xs := ["1","2","3"];
    for(x : xs){
        match x with
            0 => {break;}
            1 => {continue;}
            ;
    };
}
