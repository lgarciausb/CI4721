int main(){

    /* Debe fallar ya que la secuenciación no está hecha de forma correcta (Es decir, debe ser acción ; acción) */

    bool x := false;
    match x with
        false => {return 5;}
        true  => {return 7;}
    ;
    9;
    
    return 0;
}
