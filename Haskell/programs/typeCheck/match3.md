int main(){
    /* Debe fallar porque usa acciones/keywords de un for sin estar en un for */
    int x := 3;
    match x with
        0 => {break;}
        1 => {continue;}
    ;
}
