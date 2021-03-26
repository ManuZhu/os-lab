#ifndef _MEMDEV_H_
#define _MEMDEV_H_
 
#ifndef MEMDEV_MAJOR
#define MEMDEV_MAJOR 217   /*预设的mem的主设备号*/
#endif
 
#ifndef MEMDEV_NR_DEVS
#define MEMDEV_NR_DEVS 1    /*设备数*/
#endif
 
#ifndef BUFFER_SIZE
#define BUFFER_SIZE 32
#endif
 
/*mem设备描述结构体*/
struct mem_dev{
  struct cdev *cdev;                                                        
  wait_queue_head_t readq;
  wait_queue_head_t writeq;      
};
 
#endif /* _MEMDEV_H_ */
