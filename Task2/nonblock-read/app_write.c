#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
 
int main(){
    int fp;
    char Buf[128];

    fp = open("/dev/memdev0", O_RDWR | O_NONBLOCK);
    if(fp == -1){
        printf("Open memdev0 Error!\n");
        return -1;
    }

    while(1){
        scanf("%s", Buf);
        int len = strlen(Buf);
        int tmp = write(fp, Buf, len);
        printf("%d\n", tmp);
    }

    close(fp);
    return 0;    
}
