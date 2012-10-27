///////////////////////////////////////////////////////////////////////////////
///
/// Copyright (c) 2012 - <company name here>
///
/// Original filename: Driver10.cpp
/// Project          : Driver10
/// Date of creation : 2012-03-13
/// Author(s)        : <author name(s)>
///
/// Purpose          : <description>
///    
/// Revisions:    
///  0000 [2012-03-13] Initial revision.
///
///////////////////////////////////////////////////////////////////////////////

// $Id$

#ifdef __cplusplus
extern "C" {
#endif

#include "Ntifs.h"
#include <ntddk.h>
#include "C:\WinDDK\7600.16385.1\inc\wdf\kmdf\1.9\wdf.h"

#include <string.h>
#ifdef __cplusplus
}; // extern "C"
#endif

#include "Driver10.h"
#include "SIOCTL.H"



#define NT_DEVICE_NAME      L"\\Device\\SIOCTL"
#define DOS_DEVICE_NAME     L"\\DosDevices\\IoctlTest"

#if DBG
#define SIOCTL_KDPRINT(_x_) \
                DbgPrint("SIOCTL.SYS: ");\
                DbgPrint _x_;

#else
#define SIOCTL_KDPRINT(_x_)
#endif




#ifdef __cplusplus
namespace { // anonymous namespace to limit the scope of this global variable!
#endif
PDRIVER_OBJECT pdoGlobalDrvObj = 0;
#ifdef __cplusplus
}; // anonymous namespace
#endif


//portio




//
// The device context performs the same job as
// a WDM device extension in the driver frameworks
//
typedef struct _DEVICE_CONTEXT
{
    PVOID PortBase;       // base port address
    ULONG PortCount;      // Count of I/O addresses used.
    ULONG PortMemoryType;
    BOOLEAN PortWasMapped;  // If TRUE, we have to unmap on unload
} DEVICE_CONTEXT, *PDEVICE_CONTEXT;

WDF_DECLARE_CONTEXT_TYPE_WITH_NAME(DEVICE_CONTEXT, PortIOGetDeviceContext)

NTSTATUS
PortIOQueueInitialize(
    __in WDFDEVICE Device
    );


NTSTATUS
PortIOEvtDevicePrepareHardware(
    __in WDFDEVICE  Device,    
    __in WDFCMRESLIST  ResourcesRaw,    
    __in WDFCMRESLIST  ResourcesTranslated    
    )
/*++

Routine Description:

    This event is called by the Framework when the device is started
    or restarted after a suspend operation.

Arguments:

    Device - Handle to a framework device object.

Return Value:

    NTSTATUS - Failures will result in the device stack being torn down.

--*/
{
    ULONG  i;
    PCM_PARTIAL_RESOURCE_DESCRIPTOR  desc;
    PCM_PARTIAL_RESOURCE_DESCRIPTOR  descTranslated;
    PDEVICE_CONTEXT deviceContext = NULL;
    NTSTATUS status = STATUS_SUCCESS;

    PAGED_CODE();
    
    if ((NULL == ResourcesRaw) || 
        (NULL == ResourcesTranslated)){
        return STATUS_DEVICE_CONFIGURATION_ERROR;        
    }
        
    deviceContext = PortIOGetDeviceContext(Device);
    
    for (i=0; i < WdfCmResourceListGetCount(ResourcesRaw); i++) {
        
        desc = WdfCmResourceListGetDescriptor(ResourcesRaw, i);
        descTranslated =  WdfCmResourceListGetDescriptor(ResourcesTranslated, i);
        
        switch(desc ->Type) {
        case CmResourceTypePort:

            switch(descTranslated -> Type) {
            case CmResourceTypePort:
                deviceContext -> PortWasMapped = FALSE;
                deviceContext -> PortBase = ULongToPtr(descTranslated->u.Port.Start.LowPart);
                deviceContext -> PortCount = descTranslated ->u.Port.Length;
                KdPrint(("Resource Translated Port: (%x) Length: (%d)\n",
                         descTranslated->u.Port.Start.LowPart,
                         descTranslated->u.Port.Length));                        
                break;
            case CmResourceTypeMemory:
                //
                // Map the memory
                //
                deviceContext-> PortBase = (PVOID)
                                    MmMapIoSpace(descTranslated->u.Memory.Start,
                                                 descTranslated->u.Memory.Length,
                                                 MmNonCached);
                deviceContext-> PortCount = descTranslated->u.Memory.Length;

                deviceContext-> PortWasMapped = TRUE;
                
                KdPrint(("Resource Translated Memory: (%x) Length: (%d)\n",
                         descTranslated->u.Memory.Start.LowPart,
                         descTranslated->u.Memory.Length));                        
                break;
            default:
                KdPrint(("Unhandled resource_type (0x%x)\n", descTranslated->Type));
                status = STATUS_UNSUCCESSFUL;
                ASSERTMSG("Unhandled resource_type in start request\n", FALSE);                        
            }
            break;

        case CmResourceTypeMemory:
            deviceContext-> PortBase = (PVOID)
                                    MmMapIoSpace (descTranslated->u.Memory.Start,
                                    descTranslated->u.Memory.Length,
                                    MmNonCached);
            deviceContext-> PortCount = descTranslated->u.Memory.Length;

            deviceContext-> PortWasMapped = TRUE;
                    
            KdPrint(("Resource Translated Memory: (%x) Length: (%d)\n",
                    descTranslated->u.Memory.Start.LowPart,
                    descTranslated->u.Memory.Length));                        
            
            break;
        case CmResourceTypeInterrupt:
        default:
        KdPrint(("Unhandled resource type (0x%x)\n", desc->Type));
        status = STATUS_UNSUCCESSFUL;
        break;
        }

    }

    return status;
}

NTSTATUS
PortIOEvtDeviceReleaseHardware(    
    __in WDFDEVICE  Device,    
    __in WDFCMRESLIST  ResourcesTranslated )
/*++

Routine Description:
s

Arguments:

    Device - Handle to a framework device object.

Return Value:

    NTSTATUS - The driver is not allowed to fail this function.  If it does, the
    device stack will be torn down.

--*/
{
    PDEVICE_CONTEXT deviceContext = NULL;

    UNREFERENCED_PARAMETER(ResourcesTranslated);
    
    deviceContext = PortIOGetDeviceContext(Device);

    PAGED_CODE();
    
    //
    // We received query-remove earlier so free up resources.
    //
    if (deviceContext->PortWasMapped){
        MmUnmapIoSpace(deviceContext->PortBase, deviceContext->PortCount);
        deviceContext->PortWasMapped = FALSE;
    }

    return STATUS_SUCCESS;
}





NTSTATUS
PortIODeviceCreate(
    PWDFDEVICE_INIT DeviceInit
    )
/*++

Routine Description:

    Worker routine called to create a device and its software resources.

Arguments:

    DeviceInit - Pointer to an opaque init structure. Memory for this
        structure will be freed by the framework when the WdfDeviceCreate
        succeeds. So don't access the structure after that point.

Return Value:

    NTSTATUS

--*/
{
    WDF_OBJECT_ATTRIBUTES           deviceAttributes;
    PDEVICE_CONTEXT                 deviceContext;
    WDF_PNPPOWER_EVENT_CALLBACKS    pnpPowerCallbacks;
    WDFDEVICE                       device;
    NTSTATUS                        status;
    UNICODE_STRING                  ntDeviceName;
    UNICODE_STRING                  win32DeviceName;
    WDF_FILEOBJECT_CONFIG           fileConfig;
    
    PAGED_CODE();
    
    WDF_PNPPOWER_EVENT_CALLBACKS_INIT(&pnpPowerCallbacks);

    //
    // Register pnp/power callbacks so that we can start and stop the timer as 
    // the device gets started and stopped.
    //
    pnpPowerCallbacks.EvtDevicePrepareHardware = PortIOEvtDevicePrepareHardware;
    pnpPowerCallbacks.EvtDeviceReleaseHardware = PortIOEvtDeviceReleaseHardware;
    
    //
    // Register the PnP and power callbacks. Power policy related callbacks will 
    // be registered later in SotwareInit.
    //
    WdfDeviceInitSetPnpPowerEventCallbacks(DeviceInit, &pnpPowerCallbacks);

    WDF_OBJECT_ATTRIBUTES_INIT_CONTEXT_TYPE(&deviceAttributes, DEVICE_CONTEXT);    

    WDF_FILEOBJECT_CONFIG_INIT(
                    &fileConfig,
                    WDF_NO_EVENT_CALLBACK, 
                    WDF_NO_EVENT_CALLBACK, 
                    WDF_NO_EVENT_CALLBACK // not interested in Cleanup
                    );
    
    // Let the framework complete create and close
    fileConfig.AutoForwardCleanupClose = WdfFalse;
    
    WdfDeviceInitSetFileObjectConfig(DeviceInit,
                                     &fileConfig,
                                     WDF_NO_OBJECT_ATTRIBUTES);
    //
    // Create a named deviceobject so that legacy applications can talk to us.
    // Since we are naming the object, we wouldn't able to install multiple
    // instance of this driver. Please note that as per PNP guidelines, we
    // should not name the FDO or create symbolic links. We are doing it because
    // we have a legacy APP that doesn't know how to open an interface.
    //
    RtlInitUnicodeString(&ntDeviceName, NT_DEVICE_NAME);
    
    status = WdfDeviceInitAssignName(DeviceInit,&ntDeviceName);
    if (!NT_SUCCESS(status)) {
        return status;
    }
    
    WdfDeviceInitSetDeviceType(DeviceInit, GPD_TYPE);

    //
    // Call this if the device is not holding a pagefile
    // crashdump file or hibernate file.
    //
    WdfDeviceInitSetPowerPageable(DeviceInit);

    status = WdfDeviceCreate(&DeviceInit, &deviceAttributes, &device);
    if (!NT_SUCCESS(status)) {
        return status;
    }

    //
    // Get the device context and initialize it. WdfObjectGet_DEVICE_CONTEXT is an
    // inline function generated by WDF_DECLARE_CONTEXT_TYPE macro in the
    // device.h header file. This function will do the type checking and return
    // the device context. If you pass a wrong object  handle
    // it will return NULL and assert if run under framework verifier mode.
    //
    deviceContext = PortIOGetDeviceContext(device);

    //
    // This values is based on the hardware design.
    // I'm assuming the address is in I/O space for our hardware.
    // Refer http://support.microsoft.com/default.aspx?scid=kb;en-us;Q323595
    // for more info.
    //        
    deviceContext-> PortMemoryType = 1; 
	

    //
    // Create a device interface so that application can find and talk
    // to us.
    //
    RtlInitUnicodeString(&win32DeviceName, DOS_DEVICE_NAME);
    
    status = WdfDeviceCreateSymbolicLink(
                device,
                &win32DeviceName);

    if (!NT_SUCCESS(status)) {
        return status;
    }

    //
    // Initialize the I/O Package and any Queues
    //
    status = PortIOQueueInitialize(device);

    return status;
}


VOID
PortIOIoctlReadPort(
    __in PDEVICE_CONTEXT devContext,
    __in WDFREQUEST Request,
    __in size_t OutBufferSize,
    __in size_t InBufferSize,
    __in ULONG IoctlCode)
/*++

Routine Description:
    This routine processes the IOCTLs which read from the ports.

Arguments:

    devContext        -  local device data
    Request        - WDF request object
    OutBufferSize    - Size of buffer for data to be sent to application
    InBufferSize - Size of buffer containing data from application
    IoctlCode   - The ioctl code from the IRP

Return Value:
    STATUS_SUCCESS           -- OK

    STATUS_INVALID_PARAMETER -- The buffer sent to the driver
                                was too small to contain the
                                port, or the buffer which
                                would be sent back to the driver
                                was not a multiple of the data size.

    STATUS_ACCESS_VIOLATION  -- An illegal port number was given.

--*/    
{
    NTSTATUS status = STATUS_SUCCESS;
    ULONG minDataBufferSize; // minimum output buffer size
    ULONG nPort; // Port number to read                                                
    PVOID pOutBuffer;  // Pointer to transfer buffer
                                   //  (treated as an array of longs).
    PVOID pInBuffer;

    UNREFERENCED_PARAMETER(OutBufferSize);
    UNREFERENCED_PARAMETER(InBufferSize);
    
    PAGED_CODE();
                                   
    switch (IoctlCode){
        case IOCTL_GPD_READ_PORT_UCHAR:
            minDataBufferSize = sizeof(UCHAR);
            break;
        case IOCTL_GPD_READ_PORT_USHORT:
            minDataBufferSize = sizeof(USHORT);
            break;
        case IOCTL_GPD_READ_PORT_ULONG:
            minDataBufferSize = sizeof(ULONG);
            break;
        default:
            status = STATUS_INVALID_PARAMETER;
            goto exit;    
    }
  /*  status = WdfRequestRetrieveInputBuffer(
                 Request,
                 sizeof(ULONG), 
                 &(pInBuffer),
                 NULL);*/
	status = WdfRequestRetrieveInputBuffer(
                 Request,
                 sizeof(UCHAR), 
                 &(pInBuffer),
                 NULL);
    if (!NT_SUCCESS(status)) {
        goto exit;
        
    }
    status = WdfRequestRetrieveOutputBuffer(
                                                                Request,
                                                                minDataBufferSize,
                                                                &(pOutBuffer),
                                                                NULL);
    if (!NT_SUCCESS(status)) {
        goto exit;        
    }
                                                                
    //nPort = *((PULONG)pInBuffer);
	nPort = *((PUCHAR)pInBuffer);
	KdPrint(("nPort = %d" , nPort));
	
    if (nPort >= devContext -> PortCount ||
        (nPort + minDataBufferSize) > devContext -> PortCount ||
        (((ULONG_PTR)devContext->PortBase + nPort) & (minDataBufferSize- 1)) != 0){

        status = STATUS_ACCESS_VIOLATION;
        goto exit;
    }

    if (devContext->PortMemoryType == 1){
        // Address is in I/O space
		KdPrint(("if (devContext->PortMemoryType == 1){"));
		KdPrint(("devContext->PortBase = %d" ,devContext->PortBase));
        switch (IoctlCode){
        case IOCTL_GPD_READ_PORT_UCHAR:
            *(PUCHAR)pOutBuffer = READ_PORT_UCHAR(
                            (PUCHAR)((ULONG_PTR)devContext->PortBase + nPort) );

			KdPrint(("Read *(PUCHAR)pOutBuffer= %d", *(PUCHAR)pOutBuffer));
            break;
        case IOCTL_GPD_READ_PORT_USHORT:
            *(PUSHORT)pOutBuffer = READ_PORT_USHORT(
                            (PUSHORT)((ULONG_PTR)devContext->PortBase + nPort) );
            break;
        case IOCTL_GPD_READ_PORT_ULONG:
            *(PULONG)pOutBuffer = READ_PORT_ULONG(
                            (PULONG)((ULONG_PTR)devContext->PortBase + nPort) );
            break;

        default:
            status =  STATUS_INVALID_PARAMETER;
            goto exit;


        }
    }
    else if (devContext->PortMemoryType == 0)
    {
        // Address is in Memory space
		KdPrint(("else if (devContext->PortMemoryType == 0)"));
		KdPrint(("devContext->PortBase = %d" ,devContext->PortBase));
        switch (IoctlCode)
        {
        case IOCTL_GPD_READ_PORT_UCHAR:
            *(PUCHAR)pOutBuffer = READ_REGISTER_UCHAR(
                            (PUCHAR)((ULONG_PTR)devContext->PortBase + nPort) );
            break;
        case IOCTL_GPD_READ_PORT_USHORT:
            *(PUSHORT)pOutBuffer = READ_REGISTER_USHORT(
                            (PUSHORT)((ULONG_PTR)devContext->PortBase + nPort) );
            break;
        case IOCTL_GPD_READ_PORT_ULONG:
            *(PULONG)pOutBuffer = READ_REGISTER_ULONG(
                            (PULONG)((ULONG_PTR)devContext->PortBase + nPort) );
            break;
        default:
            status = STATUS_INVALID_PARAMETER;
            goto exit;

        }
    }
    else
    {
        status = STATUS_UNSUCCESSFUL;
        goto exit;
    }

    WdfRequestCompleteWithInformation(Request, status, minDataBufferSize);
	
    return;

exit:

    WdfRequestComplete(Request, status);
}

VOID
PortIOIoctlWritePort(
    __in PDEVICE_CONTEXT devContext,
    __in WDFREQUEST Request,
    __in size_t OutBufferSize,
    __in size_t InBufferSize,
    __in ULONG IoctlCode)
    /*++

Routine Description:
    This routine processes the IOCTLs which write to the ports.

Arguments:

    devContext        -  local device data
    Request        - WDF request object
    OutBufferSize    - Size of buffer for data to be sent to application
    InBufferSize - Size of buffer containing data from application
    IoctlCode   - The ioctl code from the IRP

Return Value:
    STATUS_SUCCESS           -- OK

    STATUS_INVALID_PARAMETER -- The buffer sent to the driver
                                was too small to contain the
                                port, or the buffer which
                                would be sent back to the driver
                                was not a multiple of the data size.

    STATUS_ACCESS_VIOLATION  -- An illegal port number was given.    
    --*/
{
    NTSTATUS status = STATUS_SUCCESS;
    ULONG minDataBufferSize; // minimum output buffer size
    ULONG nPort; // Port number to read                                                
    PVOID pInBuffer;
    
    UNREFERENCED_PARAMETER(OutBufferSize);
    UNREFERENCED_PARAMETER(InBufferSize);

    PAGED_CODE();
                                   
    switch (IoctlCode){
        case IOCTL_GPD_WRITE_PORT_UCHAR:
            minDataBufferSize = sizeof(UCHAR);
            break;
        case IOCTL_GPD_WRITE_PORT_USHORT:
            minDataBufferSize = sizeof(USHORT);
            break;
        case IOCTL_GPD_WRITE_PORT_ULONG:
            minDataBufferSize = sizeof(ULONG);
            break;
        default:
            status = STATUS_INVALID_PARAMETER;
            goto exit;    
    }
    /*status = WdfRequestRetrieveInputBuffer(
                Request,
                sizeof(ULONG), 
                &( pInBuffer),
                NULL);*/

	status = WdfRequestRetrieveInputBuffer(
                Request,
                sizeof(UCHAR), 
                &( pInBuffer),
                NULL);
    if (!NT_SUCCESS(status)) {
        goto exit;
        
    }
    
   // nPort = *((PULONG)pInBuffer++);
	PUCHAR _tmp = (PUCHAR)pInBuffer;
	nPort = *_tmp++;
	KdPrint(("Write nPort = %d", nPort));

    if (nPort >= devContext -> PortCount ||
        (nPort + minDataBufferSize) > devContext -> PortCount ||
        (((ULONG_PTR)devContext->PortBase + nPort) & (minDataBufferSize- 1)) != 0){

        status = STATUS_ACCESS_VIOLATION;
        goto exit;
    }

    if (devContext->PortMemoryType == 1){
        // Address is in I/O space
		KdPrint(("if (devContext->PortMemoryType == 1){"));
		KdPrint(("devContext->PortBase = %d" ,devContext->PortBase));
        switch (IoctlCode){
        case IOCTL_GPD_WRITE_PORT_UCHAR:

			KdPrint(("*(PUCHAR)pInBuffer = %d" ,*(PUCHAR)pInBuffer));
            WRITE_PORT_UCHAR(
                (PUCHAR)((ULONG_PTR)devContext->PortBase + nPort),
                *(PUCHAR)pInBuffer);
            break;
        case IOCTL_GPD_WRITE_PORT_USHORT:
            WRITE_PORT_USHORT(
                (PUSHORT)((ULONG_PTR)devContext->PortBase + nPort),
                *(PUSHORT)pInBuffer);
            break;
        case IOCTL_GPD_WRITE_PORT_ULONG:
            WRITE_PORT_ULONG(
                (PULONG)((ULONG_PTR)devContext->PortBase + nPort),
                *(PULONG)pInBuffer);
            break;

        default:
            status =  STATUS_INVALID_PARAMETER;
            goto exit;
        }
    }
    else if (devContext->PortMemoryType == 0)
    {
        // Address is in Memory space
		KdPrint(("if (devContext->PortMemoryType == 0){"));
		KdPrint(("devContext->PortBase = %d" ,devContext->PortBase));
        switch (IoctlCode)
        {
        case IOCTL_GPD_WRITE_PORT_UCHAR:
            WRITE_REGISTER_UCHAR(
                    (PUCHAR)((ULONG_PTR)devContext->PortBase + nPort),
                    *(PUCHAR)pInBuffer);
            break;
        case IOCTL_GPD_WRITE_PORT_USHORT:
            WRITE_REGISTER_USHORT(
                    (PUSHORT)((ULONG_PTR)devContext->PortBase + nPort),
                    *(PUSHORT)pInBuffer );
            break;
        case IOCTL_GPD_WRITE_PORT_ULONG:
            WRITE_REGISTER_ULONG(
                    (PULONG)((ULONG_PTR)devContext->PortBase + nPort),
                    *(PULONG)pInBuffer );
            break;
        default:
            status = STATUS_INVALID_PARAMETER;
            goto exit;

        }
    }
    else
    {
        status = STATUS_UNSUCCESSFUL;
        goto exit;
    }

    WdfRequestCompleteWithInformation(Request, status, minDataBufferSize);
    return;
    
exit:
    WdfRequestComplete(Request, status);
    return;
}




VOID
PortIOEvtIoDeviceControl(
    __in WDFQUEUE     Queue,
    __in WDFREQUEST   Request,
    __in size_t       OutputBufferLength,
    __in size_t       InputBufferLength,
    __in ULONG        IoControlCode
    )
{

    PDEVICE_CONTEXT devContext = NULL;
    WDFDEVICE device;
    NTSTATUS status;

    PAGED_CODE();
    
    device = WdfIoQueueGetDevice(Queue);
    
    devContext = PortIOGetDeviceContext(device);
    
    switch(IoControlCode){
        case IOCTL_GPD_READ_PORT_UCHAR:
        case IOCTL_GPD_READ_PORT_USHORT:
        case IOCTL_GPD_READ_PORT_ULONG:
            PortIOIoctlReadPort(devContext,
                                Request,
                                OutputBufferLength,
                                InputBufferLength,
                                IoControlCode);
            break;


        case IOCTL_GPD_WRITE_PORT_UCHAR:
        case IOCTL_GPD_WRITE_PORT_USHORT:
        case IOCTL_GPD_WRITE_PORT_ULONG:    
            PortIOIoctlWritePort(devContext,
                                 Request,
                                 OutputBufferLength,
                                 InputBufferLength,
                                 IoControlCode);
            break;

        default:
            status = STATUS_INVALID_PARAMETER;
            WdfRequestComplete(Request, status);

            break;

                               
    }
        
}


NTSTATUS
PortIOQueueInitialize(
    __in WDFDEVICE Device
    )
/*++


--*/
{
    WDFQUEUE queue;
    NTSTATUS status = STATUS_SUCCESS;
    WDF_IO_QUEUE_CONFIG    queueConfig;

   PAGED_CODE();

    WDF_IO_QUEUE_CONFIG_INIT_DEFAULT_QUEUE(
        &queueConfig,
        WdfIoQueueDispatchSequential
        );

    queueConfig.EvtIoDeviceControl = PortIOEvtIoDeviceControl;    

   
    status = WdfIoQueueCreate(
                 Device,
                 &queueConfig,
                 WDF_NO_OBJECT_ATTRIBUTES,
                 &queue
                 );


    return status;
}

//portio


VOID CreateFileTest()   
{  
 NTSTATUS ntStatus;  
 OBJECT_ATTRIBUTES objectAttributes;  
 IO_STATUS_BLOCK iostatus;  
 HANDLE hfile;  
 UNICODE_STRING  SymbolFileName;  
  KdPrint(("\n-----------------创建文件测试  --------------------!\n"));  
 //初始化UNICODE_STRING字符串  
  
   //RtlInitUnicodeString(&SymbolFileName,L"\\Device\\HarddiskVolume1\\CreateFileTest.txt");  
   RtlInitUnicodeString( &SymbolFileName,  L"\\??\\d:\\\CreateFileTest.txt");//可用此行替换上一行  
 //初始化objectAttributes  
 InitializeObjectAttributes(  
     &objectAttributes, //指定一个需要OBJECT_ATTRIBUTES结构地址  
     &SymbolFileName,//是一个UNICODE_STRING字串地址，指定需要操作对象名（在这里可以是符号链接名，或者设备名)  
     OBJ_CASE_INSENSITIVE, //指定此值表示 不区分大小写。  
     NULL,   
     NULL );  
 //创建文件  
 ntStatus = ZwCreateFile(   
  &hfile,   
//PHANDLE类型指针 用于返回打开文件的句柄  
  GENERIC_WRITE,  
//ACCESS_MASK类型 此值用于描述打开文件操作（读，写，或者其它）  
  &objectAttributes,   
//此值是OBJECT_ATTRIBUTES结构的地址，该结构包含要打开的文件名 需要用InitializeObjectAttributes进行初始化  
  &iostatus,  
//指向一个IO_STATUS_BLOCK结构，返回值之一 用于存放ZwCreateFile操作的结果状态  
  NULL,  
  //PLARGE_INTEGER 类型（64位整数指针）该数指定文件初始分配时的大小。该参数如果为NULL，那么文件长度将从0开始，随着写入而增长  
  FILE_ATTRIBUTE_NORMAL,  
  //此参数在驱动下指定为0或者FILE_ATTRIBUTE_NORMAL，如果文件不是被创建和覆盖写入 则此参数将被忽略  
  FILE_SHARE_READ,  
  //指定共享模式 有共享读FILE_SHARE_READ,写FILE_SHARE_WRITE,删除FILE_SHARE_DELETE这几中模式  
  FILE_OPEN_IF,  
  //此值表示 文件存在则打开 不存在则创建一个新的文件  
  FILE_SYNCHRONOUS_IO_NONALERT,  
  //指定文件创建或者打开的附加标志 FILE_SYNCHRONOUS_IO_NONALERT表示在文件中的所有操作均同步，并没有警报  
  NULL,   
  //对于设备和中间驱动程序，此参数必须是NULL  
  0 );    
 //对于设备和中间驱动程序，此参数必须是0  
 if ( NT_SUCCESS(ntStatus))  
 {  
  KdPrint(("创建文件成功!\n"));  
 }else  
 {  
  KdPrint(("创建文件失败!\n"));  
 }  
  
 MySafeFunction();
 //文件操作  
 //.......  
  
 //关闭文件句柄  
 ZwClose(hfile);  
  
}  

NTSTATUS DRIVER10_DispatchCreateClose(
    IN PDEVICE_OBJECT		DeviceObject,
    IN PIRP					Irp
    )
{
    NTSTATUS status = STATUS_SUCCESS;
    Irp->IoStatus.Status = status;
    Irp->IoStatus.Information = 0;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return status;
}

NTSTATUS DRIVER10_DispatchDeviceControl(
    IN PDEVICE_OBJECT		DeviceObject,
    IN PIRP					Irp
    )
{
    NTSTATUS status = STATUS_SUCCESS;
    PIO_STACK_LOCATION irpSp = IoGetCurrentIrpStackLocation(Irp);

    switch(irpSp->Parameters.DeviceIoControl.IoControlCode)
    {
	case IOCTL_READ_DEVICE_INFO:
		DbgPrint("IOCTL_READ_DEVICE_INFO rev");
		break;
    case IOCTL_DRIVER10_OPERATION:
        // status = SomeHandlerFunction(irpSp);
        break;
    default:
        Irp->IoStatus.Status = STATUS_INVALID_DEVICE_REQUEST;
        Irp->IoStatus.Information = 0;
        break;
    }

    status = Irp->IoStatus.Status;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return status;
}

VOID DRIVER10_DriverUnload(
    IN PDRIVER_OBJECT		DriverObject
    )
{

	DbgPrint("first: Driver is unloading…\r\n"); 
    PDEVICE_OBJECT pdoNextDeviceObj = pdoGlobalDrvObj->DeviceObject;
    IoDeleteSymbolicLink(&usSymlinkName);

    // Delete all the device objects
    while(pdoNextDeviceObj)
    {
        PDEVICE_OBJECT pdoThisDeviceObj = pdoNextDeviceObj;
        pdoNextDeviceObj = pdoThisDeviceObj->NextDevice;
        IoDeleteDevice(pdoThisDeviceObj);
    }
}


extern "C" NTSTATUS PortIOEvtDeviceAdd(    __in WDFDRIVER       Driver,    __inout PWDFDEVICE_INIT DeviceInit    )
{
    NTSTATUS status;

    UNREFERENCED_PARAMETER(Driver);

    PAGED_CODE();

    KdPrint(("Enter PortIoDeviceAdd\n"));

    status = PortIODeviceCreate(DeviceInit);
	status = STATUS_SUCCESS;
    return status;
}




//#pragma alloc_text (PAGE, PortIOEvtDeviceAdd)


#ifdef __cplusplus
extern "C" {
#endif
NTSTATUS DriverEntry(
    IN OUT PDRIVER_OBJECT   DriverObject,
    IN PUNICODE_STRING      RegistryPath
    )
{

	WDF_DRIVER_CONFIG config;


    WDF_DRIVER_CONFIG_INIT(&config,
                        PortIOEvtDeviceAdd
                        );
    NTSTATUS        ntStatus;
	//portio
	ntStatus = WdfDriverCreate(DriverObject,
                            RegistryPath,
                            WDF_NO_OBJECT_ATTRIBUTES,
                            &config,
                            WDF_NO_HANDLE);
    if (!NT_SUCCESS(ntStatus)) {
        KdPrint(("Error: WdfDriverCreate failed 0x%x\n", ntStatus));
        return ntStatus;
    }


    return ntStatus;
	//portio 
	KdPrint(("Driver Frameworks NONPNP Legacy Driver Example\n"));
    PDEVICE_OBJECT pdoDeviceObj = 0;
    NTSTATUS status = STATUS_UNSUCCESSFUL;
    pdoGlobalDrvObj = DriverObject;

	

    UNICODE_STRING  ntUnicodeString;    // NT Device Name "\Device\SIOCTL"
    UNICODE_STRING  ntWin32NameString;    // Win32 Name "\DosDevices\IoctlTest"
    PDEVICE_OBJECT  deviceObject = NULL;    // ptr to device object

	UNREFERENCED_PARAMETER(RegistryPath);

    RtlInitUnicodeString( &ntUnicodeString, NT_DEVICE_NAME );

	DbgPrint("first: Hello World!"); 
    // Create the device object.
    //if(!NT_SUCCESS(status = IoCreateDevice(
    //    DriverObject,
    //    0,
    //    &usDeviceName,
    //    FILE_DEVICE_UNKNOWN,
    //    FILE_DEVICE_SECURE_OPEN,
    //    FALSE,
    //    &pdoDeviceObj
    //    )))
    //{
    //    // Bail out (implicitly forces the driver to unload).
    //    return status;
    //};

	//CreateFileTest(); //dd comment
	ntStatus = IoCreateDevice(
        DriverObject,                   // Our Driver Object
        0,                              // We don't use a device extension
        &ntUnicodeString,               // Device name "\Device\SIOCTL"
        FILE_DEVICE_UNKNOWN,            // Device type
        FILE_DEVICE_SECURE_OPEN,     // Device characteristics
        FALSE,                          // Not an exclusive device
        &deviceObject );                // Returned ptr to Device Object

    if ( !NT_SUCCESS( ntStatus ) )
    {
        SIOCTL_KDPRINT(("Couldn't create the device object\n"));
        return ntStatus;
    }
    // Now create the respective symbolic link object
   /* if(!NT_SUCCESS(status = IoCreateSymbolicLink(
        &usSymlinkName,
        &usDeviceName
        )))
    {
        IoDeleteDevice(pdoDeviceObj);
        return status;
    }*/

	 RtlInitUnicodeString( &ntWin32NameString, DOS_DEVICE_NAME );

    //
    // Create a symbolic link between our device name  and the Win32 name
    //
	 //ULONG ul = 10;
	 //ULONG n = RtlRandom(&ul);
	 //DbgPrint("RtlRandom start   ");
	
		//DbgPrint("%d", n);
		//for(int i = 0; i < 10 ; i++)
		//{
		//	DbgPrint("random");
		//	RtlRandom(&ul);
		//	
		//	//ul = ul % 50 + 100;
		//	DbgPrint( "%d", ul);

		//	n = ul % 50 + 100;
		//	DbgPrint( "%d", n);
		//}
		//DbgPrint("   RtlRandom end");
    ntStatus = IoCreateSymbolicLink(
                        &ntWin32NameString, &ntUnicodeString );

    if ( !NT_SUCCESS( ntStatus ) )
    {
        //
        // Delete everything that this routine has allocated.
        //
        SIOCTL_KDPRINT(("Couldn't create symbolic link\n"));
        IoDeleteDevice( deviceObject );
    }


    // NOTE: You need not provide your own implementation for any major function that
    //       you do not want to handle. I have seen code using DDKWizard that left the
    //       *empty* dispatch routines intact. This is not necessary at all!
    DriverObject->MajorFunction[IRP_MJ_CREATE] =
    DriverObject->MajorFunction[IRP_MJ_CLOSE] = DRIVER10_DispatchCreateClose;
    DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = DRIVER10_DispatchDeviceControl;
    DriverObject->DriverUnload = DRIVER10_DriverUnload;

    return STATUS_SUCCESS;
}
#ifdef __cplusplus
}; // extern "C"
#endif
