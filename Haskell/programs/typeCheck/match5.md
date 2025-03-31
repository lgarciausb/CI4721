int main(){

    /* Debe fallar ya que en este caso el match pertenece a una acción, así que debe tener returns o acciones nada más */
    bool x := false;
    match x with
        false => {5;}
        true  => {return 7;}
        ;
    return 0;
}
