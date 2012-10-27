///////////////////////////////////////////////////////////////////////////////
///
/// Copyright (c) 2012 - <company name here>
///
/// Original filename: Driver10.h
/// Project          : Driver10
/// Date of creation : <see Driver10.cpp>
/// Author(s)        : <see Driver10.cpp>
///
/// Purpose          : <see Driver10.cpp>
///
/// Revisions:         <see Driver10.cpp>
///
///////////////////////////////////////////////////////////////////////////////

// $Id$

#ifndef __DRIVER10_H_VERSION__
#define __DRIVER10_H_VERSION__ 100

#if defined(_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif


#include "drvcommon.h"
#include "drvversion.h"

#define DEVICE_NAME			"\\Device\\DRIVER10_DeviceName"
#define SYMLINK_NAME		"\\DosDevices\\DRIVER10_DeviceName"
PRESET_UNICODE_STRING(usDeviceName, DEVICE_NAME);
PRESET_UNICODE_STRING(usSymlinkName, SYMLINK_NAME);

#ifndef FILE_DEVICE_DRIVER10
#define FILE_DEVICE_DRIVER10 0x8000
#endif

// Values defined for "Method"
// METHOD_BUFFERED
// METHOD_IN_DIRECT
// METHOD_OUT_DIRECT
// METHOD_NEITHER
// 
// Values defined for "Access"
// FILE_ANY_ACCESS
// FILE_READ_ACCESS
// FILE_WRITE_ACCESS

const ULONG IOCTL_DRIVER10_OPERATION = CTL_CODE(FILE_DEVICE_DRIVER10, 0x01, METHOD_BUFFERED, FILE_READ_DATA | FILE_WRITE_DATA);

#define IOCTL_READ_DEVICE_INFO CTL_CODE(FILE_DEVICE_UNKNOWN, 0x800, METHOD_BUFFERED, FILE_ANY_ACCESS)

#endif // __DRIVER10_H_VERSION__
