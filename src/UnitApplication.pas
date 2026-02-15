unit UnitApplication;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasDblStrUtils,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasRISCV;

const ApplicationTag='riscvemu';      

type TApplication=class(TpvApplication)
      public
      private
       fDebuggerPort:TpvInt32;
       fDebuggerLocal:Boolean;
       fCountHARTs:TpvSizeInt;
       fMemorySize:TpvUInt64;
       fBIOSFileName:TpvUTF8String;
       fKernelFileName:TpvUTF8String;
       fInitrdFileName:TpvUTF8String;
       fVirtIOBlockImageFileName:TpvUTF8String;
       fNVMeImageFileName:TpvUTF8String;
       fBootArguments:TpvUTF8String;
       fAIA:Boolean;
       fDisplayMode:TPasRISCV.TDisplayMode;
       fRTCMode:TPasRISCV.TRTCMode;
       fI2CMode:TPasRISCV.TI2CMode;
      public
       constructor Create; override;
       destructor Destroy; override;
       procedure Setup; override;
       procedure Start; override;
       procedure Stop; override;
       procedure Load; override;
       procedure Unload; override;
       procedure AfterCreateSwapChain; override;
       procedure BeforeDestroySwapChain; override;
       procedure Resume; override;
       procedure Pause; override;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       procedure Update(const aDeltaTime:TpvDouble); override;
       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;
      published
       property DebuggerPort:TpvInt32 read fDebuggerPort write fDebuggerPort;
       property DebuggerLocal:Boolean read fDebuggerLocal write fDebuggerLocal;
       property CountHARTs:TpvSizeInt read fCountHARTs write fCountHARTs;
       property MemorySize:TpvUInt64 read fMemorySize write fMemorySize;
       property BIOSFileName:TpvUTF8String read fBIOSFileName write fBIOSFileName;
       property KernelFileName:TpvUTF8String read fKernelFileName write fKernelFileName;
       property InitrdFileName:TpvUTF8String read fInitrdFileName write fInitrdFileName;
       property VirtIOBlockImageFileName:TpvUTF8String read fVirtIOBlockImageFileName write fVirtIOBlockImageFileName;
       property NVMeImageFileName:TpvUTF8String read fNVMeImageFileName write fNVMeImageFileName;
       property BootArguments:TpvUTF8String read fBootArguments write fBootArguments;
       property AIA:Boolean read fAIA write fAIA;
       property DisplayMode:TPasRISCV.TDisplayMode read fDisplayMode write fDisplayMode;
       property RTCMode:TPasRISCV.TRTCMode read fRTCMode write fRTCMode;
       property I2CMode:TPasRISCV.TI2CMode read fI2CMode write fI2CMode;
     end;

var Application:TApplication=nil;

implementation

uses UnitScreenEmulator;

// Converts a human readable amount string (b, kb, kib, mb, mib, gb, gib, tb, tib) to a size, and if unit-less, assume bytes
function AmountToSize(const aAmount:TpvRawByteString;const aDefault:TpvUInt64):TpvUInt64;
var Index:TpvSizeInt;
    Value,Digits,FracValue,FracDigits,FracTenPower,Factor:TpvUInt64;
    UnitString:TpvRawByteString;    
begin

 Index:=1; 

 // Skip whitespace
 while (Index<=length(aAmount)) and (aAmount[Index] in [#1..#32]) do begin
  inc(Index);
 end;

 // Search unit string
 Value:=0;
 Digits:=0;
 while (Index<=length(aAmount)) and (aAmount[Index] in ['0'..'9']) do begin
  Value:=(Value*10)+(ord(aAmount[Index])-ord('0'));
  inc(Digits);
  inc(Index);
 end;
 if (Index<=length(aAmount)) and (aAmount[Index]='.') then begin
  inc(Index);
  FracDigits:=0;
  FracValue:=0;
  FracTenPower:=1;
  while (Index<=length(aAmount)) and (aAmount[Index] in ['0'..'9']) do begin
   FracValue:=(FracValue*10)+(ord(aAmount[Index])-ord('0'));
   FracTenPower:=FracTenPower*10;
   inc(FracDigits);
   inc(Digits);
   inc(Index);
  end;
 end else begin
  FracValue:=0;
  FracDigits:=0;
 end;

 // Skip whitespace
 while (Index<=length(aAmount)) and (aAmount[Index] in [#1..#32]) do begin
  inc(Index);
 end;

 // Get unit string
 UnitString:='';
 while (Index<=length(aAmount)) and not (aAmount[Index] in ['a'..'z','A'..'Z']) do begin
  UnitString:=UnitString+aAmount[Index];
  inc(Index);
 end;
 UnitString:=LowerCase(UnitString);

 if UnitString='kb' then begin
  Factor:=TpvUInt64(1000);
 end else if UnitString='kib' then begin
  Factor:=TpvUInt64(1024); 
 end else if UnitString='mb' then begin
  Factor:=TpvUInt64(1000000);
 end else if UnitString='mib' then begin
  Factor:=TpvUInt64(1048576);
 end else if UnitString='gb' then begin
  Factor:=TpvUInt64(1000000000);
 end else if UnitString='gib' then begin
  Factor:=TpvUInt64(1073741824);
 end else if UnitString='tb' then begin
  Factor:=TpvUInt64(1000000000000);
 end else if UnitString='tib' then begin
  Factor:=TpvUInt64(1099511627776);
 end else begin
  Factor:=TpvUInt64(1);
 end;

 if Digits>0 then begin
  result:=Value*Factor;
  if FracDigits>0 then begin
   result:=result+((FracValue*Factor) div FracTenPower);
  end;
 end else begin
  result:=aDefault;
 end;

end;

constructor TApplication.Create;
var Index,Count:TpvSizeInt;
    Parameter,Value:String; 
begin
 inherited Create;
 
 Application:=self;

 fDebuggerPort:=-1; // -1 means no debugger

 fDebuggerLocal:=false;

 fCountHARTs:=2;

 fMemorySize:=TpvUInt64(2) shl 30; // 2 GiB

 fBIOSFileName:='fw_jump.bin';

 fKernelFileName:='kernel.bin';

 fInitrdFileName:='';

 fVirtIOBlockImageFileName:='';

 fNVMeImageFileName:='';

 fBootArguments:='root=/dev/mem rw earlyprintk console=$LINUXUART$ console=tty0 earlycon=sbi';

 fAIA:=false;

 fDisplayMode:=TPasRISCV.TDisplayMode.SimpleFB;

 fRTCMode:=TPasRISCV.TRTCMode.Goldfish;

 fI2CMode:=TPasRISCV.TI2CMode.DesignWare;

 Index:=1;
 Count:=ParamCount;
 while Index<=Count do begin
  Parameter:=ParamStr(Index);
  inc(Index);
  if (length(Parameter)>0) and (Parameter[1] in ['-','/']) then begin
   Parameter:=LowerCase(Copy(Parameter,2,length(Parameter)-1));
   if (length(Parameter)>0) and (Parameter[1] in ['-','/']) then begin
    Parameter:=LowerCase(Copy(Parameter,2,length(Parameter)-1));
   end;
   if Parameter='gdb' then begin
    if Index<=Count then begin
     fDebuggerPort:=StrToIntDef(ParamStr(Index),-1);
     inc(Index);
    end;
   end else if Parameter='debug' then begin
    fDebuggerLocal:=true;
   end else if Parameter='no-debug' then begin
    fDebuggerLocal:=false;
   end else if (Parameter='smp') or (Parameter='cpucores') or (Parameter='harts') then begin
    if Index<=Count then begin
     fCountHARTs:=Max(StrToIntDef(ParamStr(Index),1),1);
     inc(Index);
    end;
   end else if (Parameter='memory') or (Parameter='ram') or (Parameter='mem') then begin
    if Index<=Count then begin
     fMemorySize:=AmountToSize(ParamStr(Index),TpvUInt64(2) shl 30);
     inc(Index);
    end;
   end else if Parameter='bios' then begin
    if Index<=Count then begin
     fBIOSFileName:=ParamStr(Index);
     inc(Index);
    end;
   end else if Parameter='kernel' then begin
    if Index<=Count then begin
     fKernelFileName:=ParamStr(Index);
     inc(Index);
    end;
   end else if Parameter='no-kernel' then begin
    fKernelFileName:='';
   end else if Parameter='initrd' then begin
    if Index<=Count then begin
     fInitrdFileName:=ParamStr(Index);
     inc(Index);
    end;
   end else if Parameter='no-initrd' then begin
    fInitrdFileName:='';
   end else if Parameter='virtioblock' then begin
    if Index<=Count then begin
     fVirtIOBlockImageFileName:=ParamStr(Index);
     inc(Index);
    end;
   end else if Parameter='no-virtioblock' then begin
    fVirtIOBlockImageFileName:='';
   end else if Parameter='nvme' then begin
    if Index<=Count then begin
     fNVMeImageFileName:=ParamStr(Index);
     inc(Index);
    end;
   end else if Parameter='no-nvme' then begin
    fNVMeImageFileName:='';
   end else if (Parameter='bootargs') or (Parameter='bootparams') then begin
    if Index<=Count then begin
     fBootArguments:=ParamStr(Index);
     inc(Index);
    end;
   end else if Parameter='aia' then begin
    fAIA:=true;
   end else if Parameter='no-aia' then begin
    fAIA:=false;
   end else if (Parameter='display') or (Parameter='displaymode') then begin
    if Index<=Count then begin
     Value:=LowerCase(ParamStr(Index));
     if Value='simplefb' then begin
      fDisplayMode:=TPasRISCV.TDisplayMode.SimpleFB;
     end else if Value='virtiogpu' then begin
      fDisplayMode:=TPasRISCV.TDisplayMode.VirtIOGPU;
     end else if Value='bochsvbe' then begin
      fDisplayMode:=TPasRISCV.TDisplayMode.BochsVBE;
     end else if Value='cirrus' then begin
      fDisplayMode:=TPasRISCV.TDisplayMode.Cirrus;
     end;
     inc(Index);
    end;
   end else if (Parameter='rtc') or (Parameter='rtcmode') then begin
    if Index<=Count then begin
     Value:=LowerCase(ParamStr(Index));
     if Value='goldfish' then begin
      fRTCMode:=TPasRISCV.TRTCMode.Goldfish;
     end else if Value='ds1742' then begin
      fRTCMode:=TPasRISCV.TRTCMode.DS1742;
     end else if Value='ds1307' then begin
      fRTCMode:=TPasRISCV.TRTCMode.DS1307;
     end else if Value='virtio' then begin
      fRTCMode:=TPasRISCV.TRTCMode.VirtIO;
     end;
     inc(Index);
    end;
   end else if (Parameter='i2c') or (Parameter='i2cmode') then begin
    if Index<=Count then begin
     Value:=LowerCase(ParamStr(Index));
     if (Value='opencores') or (Value='oc') then begin
      fI2CMode:=TPasRISCV.TI2CMode.OpenCores;
     end else if (Value='designware') or (Value='dw') then begin
      fI2CMode:=TPasRISCV.TI2CMode.DesignWare;
     end;
     inc(Index);
    end;
   end else begin
    // Ignoring
   end;
  end;
 end;

end;

destructor TApplication.Destroy;
begin
 Application:=nil;
 inherited Destroy;
end;

procedure TApplication.Setup;
begin
 if Debugging then begin
  VulkanDebugging:=true;
  VulkanValidation:=true;
 end;
 Title:='PasRISCV-based RV64GC emulator with PasVulkan-based frontend - Copyright (C) 2024-2025, Benjamin ''BeRo'' Rosseaux';
 PathName:='riscvemu.pasvulkan';
 StartScreen:=TScreenEmulator;
 VisibleMouseCursor:=true;
 CatchMouse:=false;
 HideSystemBars:=true;
 AndroidSeparateMouseAndTouch:=true;
 UseAudio:=true;
 Blocking:=false;
 Width:=1280;
 Height:=800;
//DesiredCountSwapChainImages:=2;
//PresentMode:={$ifdef NoVSync}TpvApplicationPresentMode.Mailbox{TpvApplicationPresentMode.NoVSync}{$else}TpvApplicationPresentMode.VSync{$endif};
 PresentMode:=TpvApplicationPresentMode.Mailbox;
 MaximumFramesPerSecond:=60.0;
 VulkanAPIVersion:=VK_API_VERSION_1_0;
end;

procedure TApplication.Start;
begin
 inherited Start;
end;

procedure TApplication.Stop;
begin
 inherited Stop;
end;

procedure TApplication.Load;
begin
 inherited Load;
end;

procedure TApplication.Unload;
begin
 inherited Unload;
end;

procedure TApplication.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;
end;

procedure TApplication.BeforeDestroySwapChain;
begin
 inherited BeforeDestroySwapChain;
end;

procedure TApplication.Resume;
begin
 inherited Resume;
end;

procedure TApplication.Pause;
begin
 inherited Pause;
end;

function TApplication.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=inherited KeyEvent(aKeyEvent);
end;

procedure TApplication.Update(const aDeltaTime:TpvDouble);
begin
 Sleep(1);
 inherited Update(aDeltaTime);
end;

procedure TApplication.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
begin
 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,aWaitFence);
end;

end.
