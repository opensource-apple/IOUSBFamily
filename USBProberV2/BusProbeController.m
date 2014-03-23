/*
 *
 * @APPLE_LICENSE_HEADER_START@
 *
 * Copyright (c) 1998-2003 Apple Computer, Inc.  All Rights Reserved.
 *
 * This file contains Original Code and/or Modifications of Original Code
 * as defined in and that are subject to the Apple Public Source License
 * Version 2.0 (the 'License'). You may not use this file except in
 * compliance with the License. Please obtain a copy of the License at
 * http://www.opensource.apple.com/apsl/ and read it before using this
 * file.
 *
 * The Original Code and all software distributed under the License are
 * distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR NON-INFRINGEMENT.
 * Please see the License for the specific language governing rights and
 * limitations under the License.
 *
 * @APPLE_LICENSE_HEADER_END@
 */


#import "BusProbeController.h"

@implementation BusProbeController

- init {
    if (self = [super init]) {
        _devicesArray = [[NSMutableArray alloc] init];
		// always override this default unless Nano changes it
        [[NSUserDefaults standardUserDefaults] setBool:NO forKey:@"BusProbeSuspended"];
        _busProber = [[BusProber alloc] initWithListener:self devicesArray:_devicesArray];
    }
    return self;
}

- (void)dealloc {
    [_devicesArray release];
    [_devicesArray release];
    [_busProber release];
    [super dealloc];
}

- (void)awakeFromNib {
    if ( [[NSUserDefaults standardUserDefaults] boolForKey:@"BusProbeAutoRefresh"] == NO) {
        [RefreshCheckBox setState:NSOffState];
//enable/disable button        [RefreshButton setEnabled:YES];
    } else {
        [RefreshCheckBox setState:NSOnState];
//enable/disable button        [RefreshButton setEnabled:NO];
    }
    
    if ([[NSUserDefaults standardUserDefaults] boolForKey:@"BusProbeSuspended"] == YES) {
        [SuspendCheckBox setState:NSOnState];
//enable/disable button        [RefreshButton setEnabled:YES];
    } else {
        [SuspendCheckBox setState:NSOffState];
//enable/disable button        [RefreshButton setEnabled:NO];
    }
    	
    [BusProbeOutputOV setTarget:BusProbeOutputOV];
    [BusProbeOutputOV setDoubleAction:@selector(itemDoubleClicked)];
    
 //   [self expandOutlineViewItems];
}

- (IBAction)Refresh:(id)sender
{
    [_busProber refreshData:YES];
}

- (void)generateStringFromDevices:(NSMutableString *)finalString
{
	NSEnumerator *devicesEnumerator = [_devicesArray objectEnumerator];
	BusProbeDevice *thisDevice;
	
	while (thisDevice = [devicesEnumerator nextObject]) 
    {
		[finalString appendFormat:@"%@",thisDevice];
	}
}

- (NSMutableArray *)generateArrayOfDictionariesFromDevices
{
    NSMutableArray *returnArray = [NSMutableArray array];
    NSEnumerator *devicesEnumerator = [_devicesArray objectEnumerator];
	BusProbeDevice *thisDevice;
	
	while (thisDevice = [devicesEnumerator nextObject]) 
    {
        [returnArray addObject:[thisDevice dictionaryVersionOfMe]];
    }
    return returnArray;
}

- (IBAction)SaveOutput:(id)sender
{
    NSSavePanel *sp = [NSSavePanel savePanel];
    [sp setAllowedFileTypes:[NSArray arrayWithObjects:@"txt", @"plist", nil]];
    [sp setDirectoryURL:[NSURL URLWithString:NSHomeDirectory()]];
    [sp setNameFieldStringValue:@"USB Bus Probe"];
    [sp setExtensionHidden:NO];
    
    
    NSRect exFrame;
    exFrame.origin.x=0;
    exFrame.origin.y=0;
    exFrame.size.width=200;
    exFrame.size.height=100;
    ExtensionSelector *newExSel = [[ExtensionSelector alloc] initWithFrame:exFrame];
    NSMutableDictionary *allowedTypesDictionary = [NSMutableDictionary dictionary];
    [allowedTypesDictionary setValue:@"txt" forKey:@"Text"];
    [allowedTypesDictionary setValue:@"plist" forKey:@"XML"];
    [newExSel populatePopuButtonWithArray:allowedTypesDictionary];
    [newExSel setCurrentSelection:@"Text"];
    [sp setAccessoryView:newExSel];
    newExSel.theSavePanel = sp;

    [sp beginSheetModalForWindow:[NSApp mainWindow] completionHandler:^(NSInteger returnCode){
		
        if (returnCode==NSOKButton)
        {
            NSString *selectedFileExtension = [[sp nameFieldStringValue] pathExtension];
            NSMutableString *finalString = [NSMutableString string];
            if (NSOrderedSame != [selectedFileExtension caseInsensitiveCompare:@"plist"])
            {
				[self generateStringFromDevices:finalString];
				
                if (![finalString writeToURL:[sp URL] atomically:YES encoding:NSUTF8StringEncoding error:NULL])
                {
					NSBeep();
                }
            }
            else
            {
				
                if (![[self generateArrayOfDictionariesFromDevices] writeToURL:[sp URL] atomically:YES])
                {
                    NSBeep();
				}
			}
        }
    }];
	[newExSel release];
}

- (IBAction)ToggleAutoRefresh:(id)sender
{
    NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
    
    if ([sender state] == NSOffState) {
        [defaults setBool:NO forKey:@"BusProbeAutoRefresh"];
//enable/disable button        [RefreshButton setEnabled:YES];
    } else {
        [defaults setBool:YES forKey:@"BusProbeAutoRefresh"];
//enable/disable button        [RefreshButton setEnabled:NO];
        [self Refresh:self];
    }
}

- (IBAction)ToggleProbeSuspended:(id)sender
{
    NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
    
    if ([sender state] == NSOffState) {
        [defaults setBool:NO forKey:@"BusProbeSuspended"];
    } else {
        [defaults setBool:YES forKey:@"BusProbeSuspended"];
    }
	[self Refresh:self];
}

- (void)dumpToTerminal:(NSArray*)args :(bool)showHelp
{
	NSMutableString *finalString = [[NSMutableString alloc] init];
    bool xmlOutput = NO;
    id word;	bool unKnownFilter = false; bool busProbe = false;
    BusProbeDevice *filterDevice = [[BusProbeDevice alloc] init];
    BusProbeClass  *filterDeviceClassInfo = [[BusProbeClass alloc] init];
		
    [filterDevice setDeviceClassInfo:filterDeviceClassInfo];
	if ( [args count] == 2 && !showHelp )
	{
		[self generateStringFromDevices:finalString];
	}
	else
	{
		NSEnumerator *enm = [args objectEnumerator];

        NSMutableString *usage = [[NSMutableString alloc] init];

		[usage appendFormat:@"Usage: USB Prober --busprobe [--vendorID --productID --deviceClass --deviceSubClass --deviceProtocol --XML]"];
		
		while (word = [enm nextObject]) 
		{
			if ( [word caseInsensitiveCompare:[NSString stringWithUTF8String:"--vendorID"]] == NSOrderedSame )
			{
				[filterDevice setVendorID:true];
			}
			else if ( [word caseInsensitiveCompare:[NSString stringWithUTF8String:"--productID"]] == NSOrderedSame )
			{
				[filterDevice setProductID:true];
			}
			else if ( [word caseInsensitiveCompare:[NSString stringWithUTF8String:"--deviceClass"]] == NSOrderedSame )
			{
				[filterDeviceClassInfo setClassNum:true];
			}
			else if ( [word caseInsensitiveCompare:[NSString stringWithUTF8String:"--deviceSubClass"]] == NSOrderedSame )
			{
				[filterDeviceClassInfo setSubclassNum:true];
			}
			else if ( [word caseInsensitiveCompare:[NSString stringWithUTF8String:"--deviceProtocol"]] == NSOrderedSame )
			{
				[filterDeviceClassInfo setProtocolNum:true];
			}
			else if ( [word caseInsensitiveCompare:[NSString stringWithUTF8String:"--busprobe"]] == NSOrderedSame )
			{
				busProbe = true;
			}
			else if ( [word rangeOfString:[[NSProcessInfo processInfo] processName]].length > 0 )
			{
				// do nothing here
			}
            else if ( [word caseInsensitiveCompare:[NSString stringWithUTF8String:"--XML"]] == NSOrderedSame )
            {
                xmlOutput = YES;
            }
			else if ( [word caseInsensitiveCompare:[NSString stringWithUTF8String:"--help"]] == NSOrderedSame )
			{
				showHelp = true;
				word = nil;
				break;
			}
			else
			{
				unKnownFilter = true;
				showHelp = true;
				break;
			}
		}
		
		if ( !unKnownFilter && !showHelp && busProbe)
		{
			[self applyFilter :filterDevice :finalString];
		}
		else
		{
			if ( word )
			{
				[finalString appendFormat:@"illegal option : "];
				[finalString appendFormat:@"%@ \n",word];
			}
			[finalString appendFormat:@"%@\n", usage];
		}
		[usage release];
	}
    if (!xmlOutput || unKnownFilter || showHelp)
    {
	fprintf(stdout, "%s", [finalString cStringUsingEncoding:NSUTF8StringEncoding]);
	}
    else
    {
        fprintf(stdout, "%s", [[[self strippedDownListFromFilter:filterDevice] description] UTF8String]);
    }
    [filterDevice release];
    [filterDeviceClassInfo release];
	[finalString release];
}

- (void)applyFilter :(BusProbeDevice*)filterDevice :(NSMutableString *)finalString
{
	NSEnumerator *devicesEnumerator = [_devicesArray objectEnumerator];
	BusProbeDevice *thisDevice;
	
	[finalString appendString:@"\n"];
	while ( thisDevice = [devicesEnumerator nextObject] ) 
	{
		[finalString appendFormat:@"%@ %@\n",[thisDevice deviceName],[thisDevice deviceDescription]];
		if ( ( [filterDevice productID] == true ) || ( [filterDevice vendorID] == true ) )
		{
			[finalString appendFormat:@"%@",[thisDevice descriptionForName:@"Device VendorID/ProductID:"]];
		}
		
		if ( [[filterDevice deviceClassInfo] classNum] == true )
		{
			[finalString appendFormat:@"%@",[thisDevice descriptionForName:@"Device Class:"]];
		}
		
		if ( [[filterDevice deviceClassInfo] subclassNum] == true )
		{
			[finalString appendFormat:@"%@",[thisDevice descriptionForName:@"Device Subclass:"]];
		}
		
		if ( [[filterDevice deviceClassInfo] protocolNum] == true )
		{
			[finalString appendFormat:@"%@",[thisDevice descriptionForName:@"Device Protocol:"]];
		}
	}
}

-(NSMutableArray *)strippedDownListFromFilter:(BusProbeDevice *)filterDevice
{
    NSMutableArray *returnArray = [NSMutableArray array];
    NSMutableArray *fullList = [self generateArrayOfDictionariesFromDevices];
    NSEnumerator *fullEnum = [fullList objectEnumerator];
    NSMutableDictionary* eachObject;
    BOOL doIFilter = NO;
    //NSLog(@"%i %i")
    if ([filterDevice productID]==true||[filterDevice vendorID]==true||[[filterDevice deviceClassInfo] classNum]==true||[[filterDevice deviceClassInfo] subclassNum]==true||[[filterDevice deviceClassInfo] protocolNum]==true)
    {
        doIFilter = YES;
    }
    while ((eachObject = [fullEnum nextObject]))
    {
        if (doIFilter)
        {
            NSMutableDictionary *newDict = [NSMutableDictionary dictionary];
            [newDict setValue:[[eachObject valueForKey:@"nodeData"] valueForKey:@"nodeValue"] forKey:@"Device"];
            if ([filterDevice productID] == true)
            {
                [newDict setObject:[eachObject valueForKey:@"productID"] forKey:@"productID"];
            }
            if ([filterDevice vendorID] == true)
            {
                [newDict setObject:[eachObject valueForKey:@"vendorID"] forKey:@"vendorID"];
            }
            if ([[filterDevice deviceClassInfo] classNum] == true)
            {
                [newDict setObject:[[eachObject valueForKey:@"deviceClassInfo"] valueForKey:@"classNum"] forKey:@"deviceClass"];
            }
            if ([[filterDevice deviceClassInfo] subclassNum] == true)
            {
                [newDict setObject:[[eachObject valueForKey:@"deviceClassInfo"] valueForKey:@"subclassNum"] forKey:@"deviceSubClass"];
            }
            if ([[filterDevice deviceClassInfo] protocolNum] == true)
            {
                [newDict setObject:[[eachObject valueForKey:@"deviceClassInfo"] valueForKey:@"protocolNum"] forKey:@"deviceProtocol"];
            }
            [returnArray addObject:newDict];
        }
        else
        {
            [returnArray addObject:eachObject];
        }
    }
    return returnArray;
}

- (void)busProberInformationDidChange:(BusProber *)aProber {
    [BusProbeOutputOV reloadData];
//    [self expandOutlineViewItems];
}

- (void)expandOutlineViewItems {
    NSEnumerator *enumerator;
    id thisNode;

    enumerator = [_devicesArray objectEnumerator];
    while (thisNode = [enumerator nextObject]) {
        [BusProbeOutputOV expandItem:thisNode];
    }
}

- (id)outlineView:(NSOutlineView *)ov child:(NSInteger)index ofItem:(id)item
{
    if (item == nil) {
        return [_devicesArray objectAtIndex:index];
    } else if ([item class] == [BusProbeDevice class]) {
        return [[(BusProbeDevice *)item rootNode] childAtIndex:index];
    }
    return [(OutlineViewNode *)item childAtIndex:index];
}

- (BOOL)outlineView:(NSOutlineView *)ov isItemExpandable:(id)item
{
    if ([item class] == [BusProbeDevice class]) {
        return [[(BusProbeDevice *)item rootNode] isExpandable];
    }
    return [item isExpandable];
}

- (NSInteger)outlineView:(NSOutlineView *)ov numberOfChildrenOfItem:(id)item
{
    if (item == nil) {
        return [_devicesArray count];
    } else if ([item class] == [BusProbeDevice class]) {
        return [[(BusProbeDevice *)item rootNode] childrenCount];
    }
    return [item childrenCount];
}

- (id)outlineView:(NSOutlineView *)ov objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item
{   
    if ([[tableColumn identifier] intValue] == 0) {
        if ([item class] == [BusProbeDevice class]) {
            return [[(BusProbeDevice *)item rootNode] name];
        }
        return [(OutlineViewNode *)item name];
    } else {
        if ([item class] == [BusProbeDevice class]) {
            return [[(BusProbeDevice *)item rootNode] value];
        }
        return [item value];
    }
}

@end
