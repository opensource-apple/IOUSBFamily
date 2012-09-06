/*
 * Copyright (c) 2004-2006 Apple Computer, Inc. All rights reserved.
 *
 * @APPLE_LICENSE_HEADER_START@
 * 
 * The contents of this file constitute Original Code as defined in and
 * are subject to the Apple Public Source License Version 1.2 (the
 * "License").  You may not use this file except in compliance with the
 * License.  Please obtain a copy of the License at
 * http://www.apple.com/publicsource and read it before using this file.
 * 
 * This Original Code and all software distributed under the License are
 * distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR NON-INFRINGEMENT.  
 * Please see the License for the specific language governing rights and 
 * limitations under the License.
 * 
 * @APPLE_LICENSE_HEADER_END@
 */


#include <IOKit/usb/USB.h>
#include <IOKit/usb/IOUSBLog.h>
#include <IOKit/usb/IOUSBRootHubDevice.h>
#include <IOKit/pwr_mgt/RootDomain.h>
#include <IOKit/IORegistryEntry.h>
#include <IOKit/IOHibernatePrivate.h>
#include <IOKit/acpi/IOACPIPlatformDevice.h>
#include <libkern/libkern.h>

#ifndef kACPIDevicePathKey
#define kACPIDevicePathKey			"acpi-path"
#endif

#include "AppleUSBUHCI.h"


#define super IOUSBControllerV2


// ========================================================================
#pragma mark Global variables
// ========================================================================


class AppleUHCIPwrMgmtGlobals
{
public:
    AppleUHCIPwrMgmtGlobals(); 
    ~AppleUHCIPwrMgmtGlobals();
    
    inline bool isValid( void ) const;
};

#define kUSBRemoteWakeupKey "usb_remote_wakeup"

static const OSSymbol * gUSBRemoteWakeupKey;

static AppleUHCIPwrMgmtGlobals gAppleUHCIPwrMgmtGlobals;

AppleUHCIPwrMgmtGlobals::AppleUHCIPwrMgmtGlobals()
{
    gUSBRemoteWakeupKey = OSSymbol::withCStringNoCopy( kUSBRemoteWakeupKey );
}

AppleUHCIPwrMgmtGlobals::~AppleUHCIPwrMgmtGlobals()
{
    if (gUSBRemoteWakeupKey) gUSBRemoteWakeupKey->release();
}

bool AppleUHCIPwrMgmtGlobals::isValid( void ) const
{
    return (gUSBRemoteWakeupKey);
}



// ========================================================================
#pragma mark Public power management interface
// ========================================================================



#define kUHCI_NUM_POWER_STATES 2

static IOPMPowerState powerStates[kUHCI_NUM_POWER_STATES] = {
    {
        1,  // version
        0,  // capability flags
        0,  // output power character
        0,  // input power requirement
        0,  // static power
        0,  // unbudgeted power
        0,  // power to attain this state
        0,  // time to attain this state
        0,  // settle up time
        0,  // time to lower
        0,  // settle down time
        0   // power domain budget
    },
    {
        1,  // version
        IOPMDeviceUsable,  // capability flags
        IOPMPowerOn,  // output power character
        IOPMPowerOn,  // input power requirement
        0,  // static power
        0,  // unbudgeted power
        0,  // power to attain this state
        0,  // time to attain this state
        0,  // settle up time
        0,  // time to lower
        0,  // settle down time
        0   // power domain budget
    }
};


void
AppleUSBUHCI::initForPM (IOPCIDevice *provider)
{
    USBLog(3, "AppleUSBUHCI[%p]::initForPM %p", this, provider);
    
    if (provider->getProperty("built-in") && (_errataBits & kErrataICH6PowerSequencing)) 
	{
		// The ICH6 UHCI drivers on a Transition system just magically work on sleep/wake
		// so we will just hard code those. Other systems will have to be evaluated later
        setProperty("Card Type","Built-in");
        _unloadUIMAcrossSleep = false;
    }
    else 
	{
        // This appears to be necessary
		setProperty("Card Type","PCI");
		_unloadUIMAcrossSleep = true;
    }
    
	if (_errataBits & kErrataICH6PowerSequencing)
		_powerDownNotifier = registerPrioritySleepWakeInterest(PowerDownHandler, this, 0);
    
	// if we have an ExpressCard attached (non-zero port), then we need to register for some special messages to allow us to override the Resume Enables 
	// for that port (some cards disconnect when the ExpressCard power goes away and we would like to ignore these extra detach events.
	if ((_ExpressCardPort = ExpressCardPort(provider)))
	{
		provider->callPlatformFunction(
			/* function */ "RegisterDebugDriver",
			/* waitForFunction */ false,
			/* provider nubÊ Ê */ provider,
			/* unused Ê */ (void *) this,
			/* unused Ê */ (void *) NULL,
			/* unused Ê */ (void *) NULL );
	}
	_badExpressCardAttached = false;

    registerPowerDriver(this, powerStates, kUHCI_NUM_POWER_STATES);
    changePowerStateTo(kUHCIPowerLevelRunning);
}



unsigned long
AppleUSBUHCI::maxCapabilityForDomainState ( IOPMPowerFlags domainState )
{
	if ( (domainState & IOPMPowerOn) || (domainState & kIOPMDoze) ) 
	{
		return kUHCIPowerLevelRunning;
	} else 
	{
		return kUHCIPowerLevelSuspend;
	}
}



unsigned long
AppleUSBUHCI::initialPowerStateForDomainState ( IOPMPowerFlags domainState )
{
    return kUHCIPowerLevelRunning;
}



IOReturn 
AppleUSBUHCI::setPowerState( unsigned long powerStateOrdinal, IOService* whatDevice )
{
    IOReturn				result;
    static uint32_t *		pHibernateState;
    
    USBLog(2, "AppleUSBUHCI[%p]::setPowerState(%d, %p) _saveInterrupts[%p]", this, (int)powerStateOrdinal, whatDevice, (void*)_saveInterrupts);
    
    if (_uhciBusState != kUHCIBusStateSuspended) 
	{
		USBLog(6, "AppleUSBUHCI[%p]::setPowerState  calling _workLoop->CloseGate()", this);
        _workLoop->CloseGate();
    } else 
	{
		USBLog(6, "AppleUSBUHCI[%p]::setPowerState  calling _workLoop->wake(%p)", this, (void*)_uhciBusState);
        result = _workLoop->wake(&_uhciBusState);
        if (result != kIOReturnSuccess) 
		{
            USBError(1, "AppleUSBUHCI[%p]::setPowerState - Can't wake workloop, error %p", this, (void*)result);
        }
    }
    
    switch (powerStateOrdinal) 
	{
		case kUHCIPowerLevelRunning:
			USBLog(2, "AppleUSBUHCI[%p]::setPowerState RUN - changing to running state", this);
        
			if (_idleSuspend)
			{
				USBLog(2, "AppleUSBUHCI[%p]::setPowerState RUN- from idle suspend", this);
				Run(true);
				_uhciBusState = kUHCIBusStateRunning;
				_idleSuspend = false;
				break;
			}
			
			if (_uimInitialized && pHibernateState && *pHibernateState && !_wakingFromHibernation)
			{
				USBLog(1, "AppleUSBUHCI[%p]::setPowerState RUN - pHibernateState[%d] INTR[%p] _uimInitialized[%s] _uhciAvailable[%s] _uhciBusState[%d]", this, *pHibernateState, (void*)ioRead16(kUHCI_INTR), _uimInitialized ? "true" : "false",  _uhciAvailable ? "true" : "false",  _uhciBusState);
				_wakingFromHibernation = true;						// we will clear this when we create the root hub
				Run(false);
				_uhciBusState = kUHCIBusStateOff;
				_uhciAvailable = false;					// tell the interrupt filter routine that we are off

				if ( _rootHubDevice )
				{
					USBLog(2, "AppleUSBUHCI[%p]::setPowerState - Terminating root hub in setPowerState()",  this);
					_rootHubDevice->terminate(kIOServiceRequired | kIOServiceSynchronous);
					_rootHubDevice->detachAll(gIOUSBPlane);
					_rootHubDevice->release();
					_rootHubDevice = NULL;
					USBLog(2, "AppleUSBUHCI[%p]::setPowerState - Terminated root hub in setPowerState()",  this);
				}
				IOSleep(50);				// this appears to the devices as a reset, so wait the required 50 ms
				USBLog(2,"AppleUSBUHCI[%p]::setPowerState - spawning root hub creation thread", this);
				thread_call_enter(_rootHubCreationThread);
				break;
			}
			if (_uhciBusState == kUHCIBusStateSuspended) 
			{
				if (!isInactive()) 
				{
					_idleSuspend = false;
					if (_rootHubDevice == NULL) 
					{
						USBLog(2,"AppleUSBUHCI[%p]::setPowerState - spawning root hub creation thread", this);
						thread_call_enter(_rootHubCreationThread);
					}
					else 
					{
						UIMInitializeForPowerUp();
					}
				}
			}
			
			USBLog(2, "AppleUSBUHCI[%p]::setPowerState RUN - resuming controller", this);
			ResumeController();
			USBLog(2, "AppleUSBUHCI[%p]::setPowerState RUN - enabling interrupt", this);
			EnableUSBInterrupt(true);

			_remoteWakeupOccurred = true;
			USBLog(2, "AppleUSBUHCI[%p]::setPowerState RUN - changing _uhciBusState to kUHCIBusStateRunning", this);
			_uhciBusState = kUHCIBusStateRunning;
			break;
        
		case kUHCIPowerLevelSuspend:
			USBLog(5, "%s[%p]::setPowerState SUSPEND changing to suspended state", getName(), this);

			if (_unloadUIMAcrossSleep) 
			{
				USBLog(2, "AppleUSBUHCI[%p]::setPowerState SUSPEND - Unloading UIM before going to sleep", this);
				
				if ( _rootHubDevice )
				{
					_rootHubDevice->terminate(kIOServiceRequired | kIOServiceSynchronous);
					_rootHubDevice->detachAll(gIOUSBPlane);
					_rootHubDevice->release();
					_rootHubDevice = NULL;
				}
			}
			
			USBLog(5, "AppleUSBUHCI[%p]::setPowerState SUSPEND - disabling interrupt", this);
			EnableUSBInterrupt(false);
			SuspendController();

			UIMFinalizeForPowerDown();
			
			if ( !pHibernateState )
			{
				OSData * data = OSDynamicCast(OSData, (IOService::getPMRootDomain())->getProperty(kIOHibernateStateKey));
				if (data)
				{
					pHibernateState = (uint32_t *) data->getBytesNoCopy();
				}
			}
			if (pHibernateState && *pHibernateState)
			{
				USBLog(2, "AppleUSBUHCI[%p]::setPowerState SUSPEND - pHibernateState[%d]", this, *pHibernateState);
			}
			_remoteWakeupOccurred = false;
			USBLog(5, "AppleUSBUHCI[%p]::setPowerState SUSPEND - changing _uhciBusState to kUHCIBusStateSuspended", this);
			_uhciBusState = kUHCIBusStateSuspended;
	        _idleSuspend = false;
			break;
        
		case kUHCIPowerLevelIdleSuspend:
			USBLog(2, "AppleUSBUHCI[%p]::setPowerState IDLE SUSPEND- changing to idle suspended state (really stopped)", this);
			Run(false);

			USBLog(2, "AppleUSBUHCI[%p]::setPowerState IDLE SUSPEND- changing _uhciBusState to kUHCIBusStateOff", this);
			_uhciBusState = kUHCIBusStateOff;
	        _idleSuspend = true;
			break;
        
		default:
			USBLog(1, "AppleUSBUHCI[%p]::setPowerState - unknown power state %d", this, (int)powerStateOrdinal);
			break;
		}

    if (_uhciBusState == kUHCIBusStateSuspended) 
	{
		USBLog(2, "AppleUSBUHCI[%p]::setPowerState  calling _workLoop->sleep(%p)", this, (void*)_uhciBusState);
        result = _workLoop->sleep(&_uhciBusState);
        if (result!= kIOReturnSuccess) 
		{
            USBError(1, "AppleUSBUHCI[%p]::setPowerState  - Can't sleep workloop, error 0x%x", this, result);
        }
    } else 
	{
		USBLog(2, "AppleUSBUHCI[%p]::setPowerState  calling _workLoop->OpenGate()", this);
        _workLoop->OpenGate();
    }
    
    USBLog(2, "AppleUSBUHCI[%p]::setPowerState(%d, %p)  DONE - _saveInterrupts[%p]", this, (int)powerStateOrdinal, whatDevice, (void*)_saveInterrupts);
    return IOPMAckImplied;
}



IOReturn
AppleUSBUHCI::callPlatformFunction(const OSSymbol *functionName,
                                   bool waitForFunction,
                                   void *param1, void *param2,
                                   void *param3, void *param4)
{
    USBLog(3, "%s[%p]::callPlatformFunction(%s)", getName(), this, functionName->getCStringNoCopy());
    

	if (!strcmp(functionName->getCStringNoCopy(), "SetDebugDriverPowerState"))
	{
		if (param1)
		{
			// system woke from sleep -- do nothing
		}
		else
		{
			// system is going to sleep
			//
			// check if we are the controller for an expressCard port.  If so, then we need to ignore disconnects on suspend for that port  
			// this will avoid the problem where the ExpressCard power goes away, and it looks like the device detaching -- thus waking the machine!
			if ((_badExpressCardAttached) && (_ExpressCardPort > 0) && (_errataBits & kErrataSupportsPortResumeEnable))
			{
				// set PCI_RES register to enable ports to wake the computer.
				_device->configWrite8(kUHCI_PCI_RES, 0x03 & ~(1 << (_ExpressCardPort-1)));	// clear the bit for the ExpressCardPort		
			}
		}
	}

    if (functionName == gUSBRemoteWakeupKey) 
	{
		bool *wake = (bool *)param1;
	
	if (_remoteWakeupOccurred) 
	{
	    *wake = true;
	} else 
	{
	    *wake = false;
	}
    	return kIOReturnSuccess;
    }
    
    return super::callPlatformFunction(functionName, waitForFunction, param1, param2, param3, param4);
}



// ========================================================================
#pragma mark Internal methods
// ========================================================================



void			
AppleUSBUHCI::ResumeController(void)
{
    UInt16		cmd;
	int			i;
    
    USBLog(5, "AppleUSBUHCI[%p]::ResumeController", this);
    USBLog(5, "AppleUSBUHCI[%p]::ResumeController cmd state %x, status %x", this, ioRead16(kUHCI_CMD), ioRead16(kUHCI_STS));

	cmd = ioRead16(kUHCI_CMD);
	if (cmd & kUHCI_CMD_RS)
	{
		USBLog(3, "AppleUSBUHCI[%p]::ResumeController - already running - returning", this);
		return;
	}
	
	// I need to save the existing frame list before I turn on processing so I can send SOF only for 10ms after we turn the controller on
	for (i=0;i < kUHCI_NVFRAMES; i++)
	{
		_frameList[i] |= HostToUSBLong(kUHCI_FRAME_T);
	}
			
	if (cmd & kUHCI_CMD_EGSM)
	{
		USBLog(5, "AppleUSBUHCI[%p]::ResumeController controller is globally suspended - forcing resume", this);
		cmd |= kUHCI_CMD_FGR;
		ioWrite16(kUHCI_CMD, cmd);
		cmd = ioRead16(kUHCI_CMD);
		USBLog(5, "AppleUSBUHCI[%p]::ResumeController after EGSM->FGR, state is[%p]", this, (void*)cmd);
	}
    
	if (cmd & kUHCI_CMD_FGR)
	{
		// this could either be because the remote wwakeup caused this state or because we did above
		// need to wait 20ms
		IOSleep(20);
		cmd &= ~kUHCI_CMD_FGR;
		cmd &= ~kUHCI_CMD_EGSM;
		ioWrite16(kUHCI_CMD, cmd);
	}
	USBLog(5, "AppleUSBUHCI[%p]::ResumeController starting controller", this);
	Run(true);
	
	// wait 10 ms for the device to recover
	IOSleep(10);
	
	// restore the list
	for (i=0;i < kUHCI_NVFRAMES; i++)
	{
		_frameList[i] &= ~HostToUSBLong(kUHCI_FRAME_T);
	}
    
	USBLog(5, "AppleUSBUHCI[%p]::ResumeController resume done, cmd %x, status %x ports[%p, %p]", this, ioRead16(kUHCI_CMD), ioRead16(kUHCI_STS),(void*)ReadPortStatus(0), (void*)ReadPortStatus(1));
}



void			
AppleUSBUHCI::SuspendController(void)
{
    UInt16				cmd, value;
	int					i;
    
    USBLog(5, "%s[%p]::SuspendController", getName(), this);
    USBLog(5, "%s[%p]: cmd state %x, status %x", getName(), this, ioRead16(kUHCI_CMD), ioRead16(kUHCI_STS));

    // Stop the controller
    Run(false);
    
	for (i=0; i< 2; i++)
	{
		value = ReadPortStatus(i) & kUHCI_PORTSC_MASK;
		if (value & kUHCI_PORTSC_PED)
		{
			if (value & kUHCI_PORTSC_SUSPEND)
			{
				USBLog(5, "AppleUSBUHCI[%p]::SuspendController - port[%d] is suspended [%p]", this, i, (void*)value);
			}
			else
			{
				USBLog(5, "AppleUSBUHCI[%p]::SuspendController - port[%d] is enabled but not suspended [%p]", this, i, (void*)value);
			}
		}
		else
		{
			USBLog(5, "AppleUSBUHCI[%p]::SuspendController - port[%d] is not enabled [%p]", this, i, (void*)value);
		}
		
		// only do this for controllers with overcurrent additions.
		if ((_errataBits & kErrataUHCISupportsOvercurrent) && (value & kUHCI_PORTSC_OCI))  // Is the latched Overcurrent set?
		{
			// if so, clear it or we won't suspend.
			USBLog(1, "AppleUSBUHCI[%p]::SuspendController - port[%d] had the overcurrent bit set.  Clearing it", this, i);
			WritePortStatus(i, kUHCI_PORTSC_OCI); // clear overcurrent indicator
		}
	}
    // Put the controller in Global Suspend
    cmd = ioRead16(kUHCI_CMD) & ~kUHCI_CMD_FGR;
    cmd |= kUHCI_CMD_EGSM;
    ioWrite16(kUHCI_CMD, cmd);
	_uhciBusState = kUHCIBusStateSuspended;   
    IOSleep(3);
    USBLog(5, "%s[%p]: suspend done, cmd %x, status %x", getName(), this, ioRead16(kUHCI_CMD), ioRead16(kUHCI_STS));
}



IOReturn 
AppleUSBUHCI::PowerDownHandler(void *target, void *refCon, UInt32 messageType, IOService *service, void *messageArgument, vm_size_t argSize )
{
    AppleUSBUHCI *	me = OSDynamicCast(AppleUSBUHCI, (OSObject *)target);
    
    if (!me)
        return kIOReturnUnsupported;
    
    USBLog(5, "AppleUSBUHCI[%p]::PowerDownHandler %p %p", me, (void*)messageType, messageArgument);
    switch (messageType)
    {
        case kIOMessageSystemWillRestart:
        case kIOMessageSystemWillPowerOff:
            if (me->_uhciBusState == kUHCIBusStateRunning) 
			{
                if ( me->_rootHubDevice )
                {
					USBLog(3, "AppleUSBUHCI[%p]::PowerDownHandler - terminating root hub", me);
                    me->_rootHubDevice->terminate(kIOServiceRequired | kIOServiceSynchronous);
                    me->_rootHubDevice->detachAll(gIOUSBPlane);
                    me->_rootHubDevice->release();
                    me->_rootHubDevice = NULL;
                }
				
				// Let's not look for any timeouts anymore
				// NOTE: This really should be done in the superclass, but there was no good way to do that in the time frame
				// we had. The PowerDownHandler should just be moved to the controller level
				me->_watchdogUSBTimer->cancelTimeout();
				
				me->EnableUSBInterrupt(false);
                me->Run(false);
				me->Command(kUHCI_CMD_GRESET);
                me->_uhciBusState = kUHCIBusStateOff;
            }

            // Always disable bus mastering
            me->UIMFinalizeForPowerDown();
            break;
            
        default:
            // We don't care about any other message that comes in here.
            break;
            
    }
    return kIOReturnSuccess;
}



static IOACPIPlatformDevice * CopyACPIDevice( IORegistryEntry * device )
{
	IOACPIPlatformDevice *  acpiDevice = 0;
	OSString *				acpiPath;

	if (device)
	{
		acpiPath = (OSString *) device->copyProperty(kACPIDevicePathKey);
		if (acpiPath && !OSDynamicCast(OSString, acpiPath))
		{
			acpiPath->release();
			acpiPath = 0;
		}

		if (acpiPath)
		{
			IORegistryEntry * entry;

			entry = IORegistryEntry::fromPath(acpiPath->getCStringNoCopy());
			acpiPath->release();

			if (entry && entry->metaCast("IOACPIPlatformDevice"))
				acpiDevice = (IOACPIPlatformDevice *) entry;
			else if (entry)
				entry->release();
		}
	}

	return (acpiDevice);
}

static bool HasExpressCardUSB( IORegistryEntry * acpiDevice, UInt32 * portnum )
{
	const IORegistryPlane *	acpiPlane;
	bool					match = false;
	IORegistryIterator *	iter;
	IORegistryEntry *		entry;

	do {
		acpiPlane = acpiDevice->getPlane( "IOACPIPlane" );
		if (!acpiPlane)
			break;

		// acpiDevice is the USB controller in ACPI plane.
		// Recursively iterate over children of acpiDevice.

		iter = IORegistryIterator::iterateOver(
				/* start */	acpiDevice,
				/* plane */	acpiPlane,
				/* options */ kIORegistryIterateRecursively);

		if (iter)
		{
			while (!match && (entry = iter->getNextObject()))
			{
				// USB port must be a leaf node (no child), and
				// must be an IOACPIPlatformDevice.

				if ((entry->getChildEntry(acpiPlane) == 0) &&
					entry->metaCast("IOACPIPlatformDevice"))
				{
					IOACPIPlatformDevice * port;
					port = (IOACPIPlatformDevice *) entry;

					// Express card port? Is port ejectable?

					if (port->validateObject( "_EJD" ) == kIOReturnSuccess)
					{
						// Determining the USB port number.
						if (portnum)
							*portnum = strtoul(port->getLocation(), NULL, 10);
						match = true;
					}
				}
			}

			iter->release();
		}
	}
	while (false);
	
	return match;
}

// Checks for ExpressCard connected to this controller, and returns the port number (1 based)
// Will return 0 if no ExpressCard is connected to this controller.
//
UInt32 AppleUSBUHCI::ExpressCardPort( IOService * provider )
{
	IOACPIPlatformDevice *	acpiDevice;
	UInt32					portNum = 0;
	bool					isPCIeUSB;
	
	acpiDevice = CopyACPIDevice( provider );
	if (acpiDevice)
	{
		isPCIeUSB = HasExpressCardUSB( acpiDevice, &portNum );	
		acpiDevice->release();
	}
	return(portNum);
}

