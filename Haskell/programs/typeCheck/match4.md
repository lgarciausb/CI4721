int main(){

    /* Debe fallar ya que en este caso el match pertenece a una expresión, no a una acción, por lo que no debe tener
        Return */
    bool x := false;
    int y := match x with
            false => {5;}
            true  => {return 7;}
        ;
    return 0;
}
