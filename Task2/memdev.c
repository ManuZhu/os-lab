#include <linux/module.h>
#include <linux/types.h>
#include <linux/fs.h>
#include <linux/errno.h>
#include <linux/mm.h>
#include <linux/sched.h>
#include <linux/init.h>
#include <linux/cdev.h>
#include <asm/io.h>
#include <asm/switch_to.h>
#include <asm/uaccess.h>
#include <linux/slab.h>
#include <linux/poll.h>
#include <linux/kfifo.h> 
#include <linux/wait.h>
#include "memdev.h"

static int mem_major = MEMDEV_MAJOR;
static struct mem_dev *mem_devp;

DEFINE_KFIFO(FIFOBuffer, char, BUFFER_SIZE);

/*文件打开函数*/
int mem_open(struct inode *inode, struct file *filp)
{
  printk(KERN_INFO"open:%d\n",MINOR(inode->i_rdev));
  filp->private_data = mem_devp;
  return 0; 
}
 
/*读函数*/
static ssize_t mem_read(struct file *filp, char __user *buf, size_t size, loff_t *ppos)
{
  int ret, actual_read;

  ret = actual_read = 0;

  DECLARE_WAITQUEUE(wq, current);
  add_wait_queue(&mem_devp->readq, &wq);

  if(kfifo_is_empty(&FIFOBuffer))
  {
    if(filp->f_flags & O_NONBLOCK)
    {
      remove_wait_queue(&mem_devp->readq, &wq);
      return -EAGAIN;
    }
    wait_event_interruptible(mem_devp->readq, !kfifo_is_empty(&FIFOBuffer));
  }
  
  ret = kfifo_to_user(&FIFOBuffer, buf, size, &actual_read);

  remove_wait_queue(&mem_devp->readq, &wq);

  if(!kfifo_is_full(&FIFOBuffer))
    wake_up_interruptible(&mem_devp->writeq);
  
  return !ret?actual_read:ret;
}
 
/*写函数*/
static ssize_t mem_write(struct file *filp, const char __user *buf, size_t size, loff_t *ppos)
{
  int ret, actual_write;

  ret = actual_write = 0;

  DECLARE_WAITQUEUE(wq, current);
  add_wait_queue(&mem_devp->writeq, &wq);

  if(kfifo_is_full(&FIFOBuffer))
  {
    if(filp->f_flags & O_NONBLOCK)
    {
      remove_wait_queue(&mem_devp->writeq, &wq);
      return -EAGAIN;
    }
    wait_event_interruptible(mem_devp->writeq, !kfifo_is_full(&FIFOBuffer));
  }
  
  ret = kfifo_from_user(&FIFOBuffer, buf, size, &actual_write);

  remove_wait_queue(&mem_devp->writeq, &wq);

  if(!kfifo_is_empty(&FIFOBuffer))
    wake_up_interruptible(&mem_devp->readq);
  
  return !ret?actual_write:ret;
}
 
/*文件操作结构体*/
static const struct file_operations mem_fops =
{
  .owner = THIS_MODULE,   
  .read = mem_read,  
  .write = mem_write,  
  .open = mem_open,  
};
 
/*设备驱动模块加载函数*/
static int memdev_init(void)
{
  int result;
  dev_t devno = MKDEV(mem_major, 0);
 
  /* 静态申请设备号*/
  if (mem_major)
    result = register_chrdev_region(devno, 1, "memdev");
  else /* 动态分配设备号 */
  {
    result = alloc_chrdev_region(&devno, 0, 1, "memdev");
    mem_major = MAJOR(devno);
  }  
  
  if (result < 0)
    return result;

  /* 为设备描述结构分配内存*/
  mem_devp = kmalloc(MEMDEV_NR_DEVS * sizeof(struct mem_dev), GFP_KERNEL);

  if(!mem_devp)    /*申请失败*/
  {
    result = -ENOMEM;
    unregister_chrdev_region(devno, 1);
    return result;
  }

  mem_devp->cdev = kmalloc(sizeof(struct cdev), GFP_KERNEL);
 
  /*初始化cdev结构*/
  cdev_init(mem_devp->cdev, &mem_fops);
  mem_devp->cdev->owner = THIS_MODULE;
  mem_devp->cdev->ops = &mem_fops;

  /* 注册字符设备 */
  result = cdev_add(mem_devp->cdev, MKDEV(mem_major, 0), MEMDEV_NR_DEVS);
  init_waitqueue_head(&(mem_devp->readq));
  init_waitqueue_head(&(mem_devp->writeq));
  return result;
}
 
/*模块卸载函数*/
static void memdev_exit(void)
{
  cdev_del(mem_devp->cdev);/*注销设备*/
  kfree(mem_devp);/*释放设备结构体内存*/
  unregister_chrdev_region(MKDEV(mem_major, 0), 1); /*释放设备号*/
}
 
MODULE_AUTHOR("Manu Zhu");
MODULE_LICENSE("GPL");
 
module_init(memdev_init);
module_exit(memdev_exit);
