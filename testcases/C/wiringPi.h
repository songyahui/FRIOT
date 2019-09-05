#include <time.h>

#define INPUT 0
#define OUTPUT 1

int wiringPiSetup () {
    return 1;
}

void pinMode (int a, int b){
    return;
}

char * toStr(int b){
    return "null";
}

void digitalWrite (int a, int re){
    return;
}



int digitalRead (int a ){
    return 1;
}

void delay(int milliseconds)
{
    long pause;
    clock_t now,then;

    pause = milliseconds*(CLOCKS_PER_SEC/1000);
    now = then = clock();
    while( (now-then) < pause )
        now = clock();
}

void lcdPuts (int a, char* b){
    return;
}
