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
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application;

const ApplicationTag='riscvemu';      

type TApplication=class(TpvApplication)
      public
      private
       fDebuggerPort:TpvInt32;
       fCountCPUCores:TpvSizeInt;
       fBIOSFileName:TpvUTF8String;
       fKernelFileName:TpvUTF8String;
       fInitrdFileName:TpvUTF8String;
       fVirtIOBlockImageFileName:TpvUTF8String;
       fNVMeImageFileName:TpvUTF8String;
       fBootArguments:TpvUTF8String;
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
       property CountCPUCores:TpvSizeInt read fCountCPUCores write fCountCPUCores;
       property BIOSFileName:TpvUTF8String read fBIOSFileName write fBIOSFileName;
       property KernelFileName:TpvUTF8String read fKernelFileName write fKernelFileName;
       property InitrdFileName:TpvUTF8String read fInitrdFileName write fInitrdFileName;
       property VirtIOBlockImageFileName:TpvUTF8String read fVirtIOBlockImageFileName write fVirtIOBlockImageFileName;
       property NVMeImageFileName:TpvUTF8String read fNVMeImageFileName write fNVMeImageFileName;
       property BootArguments:TpvUTF8String read fBootArguments write fBootArguments;
     end;

var Application:TApplication=nil;

implementation

uses UnitScreenEmulator;

constructor TApplication.Create;
var Index,Count:TpvSizeInt;
    Parameter:String; 
begin
 inherited Create;
 
 Application:=self;

 fDebuggerPort:=-1; // -1 means no debugger

 fCountCPUCores:=1;

 fBIOSFileName:='fw_jump.bin';

 fKernelFileName:='kernel.bin';

 fInitrdFileName:='';

 fVirtIOBlockImageFileName:='';

 fNVMeImageFileName:='';

 fBootArguments:='root=/dev/mem rw earlyprintk console=$LINUXUART$ console=tty0 earlycon=sbi';

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
   if (Parameter='debug') or (Parameter='gdb') then begin
    if Index<=Count then begin
     fDebuggerPort:=StrToIntDef(ParamStr(Index),-1);
     inc(Index);
    end;
   end else if (Parameter='smp') or (Parameter='cpucores') then begin
    if Index<=Count then begin
     fCountCPUCores:=Max(StrToIntDef(ParamStr(Index),1),1);
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
 Title:='PasRISCV-based RV64GC emulator with PasVulkan-based frontend - Copyright (C) 2024, Benjamin ''BeRo'' Rosseaux';
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
