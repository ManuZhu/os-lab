#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
 
int main(){
    int fp;
    char Buf[128];
    scanf("%s", Buf);
    int len = strlen(Buf);
    fp = open("/dev/memdev0", O_RDWR | O_NONBLOCK);
    if(fp == -1){
        printf("Open memdev0 Error!\n");
        return -1;
    }

    while(1){
        int tmp = write(fp, Buf, len);
        printf("%d\n", tmp);
        sleep(1);
    }

    close(fp);
    return 0;    
}
