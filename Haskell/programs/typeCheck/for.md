int main(){
    /* Debe fallar porque el argumento del for no es un vector. */
    string xs := "Hola Mundo";
    for(x : xs){
        match x with
            0 => {break;}
            1 => {continue;}
            ;
    };
}
