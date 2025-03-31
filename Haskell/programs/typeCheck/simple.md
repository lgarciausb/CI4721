int main(){
 

    /* En este caso, debe fallar porque no son compatibles los tipos de x & y*/
 

    int x := 3;
 

    string y := "Hola Mundo";
 

    int z := 3 + 4;
    
    float z0 := 3;

    float z1 := 3 + 4; /* error por el momento. (+) :: Z -> Z -> Z, y solo los numeros son polimorfos. */

    float z2 := 3.4 + 9;

    float z3 := 9 + 3.4;

    float z4 := 3.4 + 4.4;

    int z5 := 5.4; /* error, no coercionable */



 

    return 0;
 

}
