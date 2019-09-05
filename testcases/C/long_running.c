#include<stdio.h>
#include<string.h>
#include<pthread.h>
#include<stdlib.h>
#include<unistd.h>
#include "wiringPi.h"

#define MotionSensor 0
#define TempSensor 1
#define LCD 2
#define LED 3

typedef struct _Model{
    int motion;
    int ispeopleinside;
    float temp;
    char * time;
    char * lcd;
    int led;
}Model;

Model model = {0,0,20,"D:M:S","nothing",0}; 

void* thread1(void *arg){ //motion sensor
    printf ("thread1\n");
    pinMode (MotionSensor, INPUT) ;       
    while (1){
        int motion = digitalRead (MotionSensor);
        if (motion != model.motion) {
            model.motion = motion;
        }
        delay (1000) ;
    }
    return NULL;
}

void* thread2(void *arg){ // temp sensor
    printf ("thread2\n");
    pinMode (TempSensor, INPUT) ;  
       // aka BCM_GPIO pin 17
    while(1){
        int temp = digitalRead (TempSensor);
        if (temp != model.motion) {
            model.motion = temp;
        }

        delay (1000) ;
    }
    return NULL;
}

void* thread3(void *arg){ // time
    printf ("thread3\n");
    while (1){
        time_t timer;
        char buffer[26];
        struct tm* tm_info;
    
        time(&timer);
        tm_info = localtime(&timer);
        strftime(buffer, 26, "Time: %H:%M:%S", tm_info);
        model.time = buffer;
        delay (1000) ;
    }
    return NULL;
}

void* thread4(void *arg){
    printf ("thread4\n");
    while (1){
        char data[100] ;
        sprintf(data, "%f", model.temp); 
        strcat(data, model.time);
        strcat(data, "\n");
        lcdPuts(LCD, data);
        printf ( "%s",data);
        delay(5000);
    }
    return NULL;
}

void* thread5(void *arg){//is peopel inside sensor
    printf ("thread5\n");
    while (1){
        model.ispeopleinside = model.motion;
        delay (1000) ;
    }
    return NULL;
}

void* thread6(void *arg){//control led
    printf ("thread6\n");
    pinMode (LED, OUTPUT) ;
    while (1){
        digitalWrite(LED, model.ispeopleinside);
        delay (1000) ;
    }
    return NULL;
}

pthread_t tid[6];

int main(void)
{
    int err1, err2, err3, err4, err5, err6;

        err1 = pthread_create(&(tid[0]), NULL, &thread1, NULL);
        if (err1 != 0)
            printf("\ncan't create thread :[%s]", strerror(err1));
        else
            printf("\n Thread created successfully\n");

        err2 = pthread_create(&(tid[1]), NULL, &thread2, NULL);
        if (err2 != 0)
            printf("\ncan't create thread :[%s]", strerror(err2));
        else
            printf("\n Thread created successfully\n");

        err3 = pthread_create(&(tid[2]), NULL, &thread3, NULL);
        if (err3 != 0)
            printf("\ncan't create thread :[%s]", strerror(err3));
        else
            printf("\n Thread created successfully\n");

        err4 = pthread_create(&(tid[3]), NULL, &thread4, NULL);
        if (err4 != 0)
            printf("\ncan't create thread :[%s]", strerror(err4));
        else
            printf("\n Thread created successfully\n");

        err5 = pthread_create(&(tid[4]), NULL, &thread5, NULL);
        if (err5 != 0)
            printf("\ncan't create thread :[%s]", strerror(err5));
        else
            printf("\n Thread created successfully\n");

        err6 = pthread_create(&(tid[5]), NULL, &thread6, NULL);
        if (err6 != 0)
            printf("\ncan't create thread :[%s]", strerror(err6));
        else
            printf("\n Thread created successfully\n");

    sleep(5000);
    return 0;
}

