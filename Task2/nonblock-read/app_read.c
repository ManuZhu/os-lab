#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

int main(){
    int fp;
    char Buf[128];
    
    fp = open("/dev/memdev0", O_RDONLY | O_NONBLOCK);
    if(fp == -1){
        printf("Open memdev0 Error!\n");
        return -1;
    }

    while(1){
        int len = read(fp, Buf, 1024);
        if(len==-1) printf("%d\n",len);
        else{
            Buf[len] = 0;
            printf("%s\n", Buf);
        }
        sleep(3);
    }

    close(fp);
    return 0;
}