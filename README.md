# pasriscvemu

This is the PasVulkan-based emulator frontend for the PasRISCV RV64GC RISCV-V emulator.

## Getting Started

### Prerequisites

You need to have the following installed:

- [FreePascal](https://www.freepascal.org/) (unstable dev version) + [Lazarus](https://www.lazarus-ide.org/) or a recent version of Embarcadero Delphi  
- [PasVulkan](htttps://github.com/BeRo1985/pasvulkan) with all its dependencies and submodules (which includes PasRISCV and PasTerm already)

### Setup

Clone this pasriscemu repository with all submodules into the projects directory of the PasVulkan repository:

```bash
cd /path/to/pasvulkan/projects
git clone --recurse-submodules https://github.com/BeRo1985/pasriscvemu.git
```

But not other way around, because the pasriscvemu repository is dependent on the PasVulkan repository, because of relative paths in the project files. PasVulkan itself has already submodules for PasRISCV and PasTerm, which are also needed by pasriscvemu, where PasRISCV is the actual RISCV-V emulator core and PasTerm is the terminal emulator for the serial console when the framebuffer isn't currently active. 

### Building

Just open the `pasriscvemu.lpi` in Lazarus or 'pasriscvemu.dproj' in Delphi and compile it. The executable will be in the `bin` directory.

### Running

Just run the `pasriscvemu` executable in the `bin` directory. Otherwise look into the UnitApplication.pas file for the command line options.

### Keys

CTRL+F12 toggles between the framebuffer and the serial console.

### FAQ

#### Why is the emulator not a part of the PasRISCV repository itself?

Because pasriscvemu is just a PasVulkan-based frontend for the PasRISCV emulator core, which is a standalone library and can be used in other projects as well.

#### Why is the emulator not a part of the PasVulkan repository itself?

Because pasriscvemu is a separate project and not directly related to Vulkan, but it uses PasVulkan as a frontend for the framebuffer output.  But PasVulkan maybe get a dependency on PasRISCV in the future, for example for cross-platform "scripting" purposes and so on, but not the other way around. So PasVulkan has already submodules for PasRISCV and PasTerm, which are also needed by pasriscvemu.

### Related repositories

- [PasVulkan](https://github.com/BeRo1985/pasvulkan) - PasVulkan itself
- [PasRISCV](https://github.com/BeRo1985/pasriscv) - The RISCV-V emulator core
- [PasTerm](https://github.com/BeRo1985/pasterm) - The terminal emulator for the serial console
- [PasMP](https://github.com/Bero1985/pasmp) - A parallel-processing/multi-processing library for Object Pascal
- [RNL](https://github.com/Bero1985/rnl) - RNL - Realtime Network Library - The opensource reliable UDP network library, including cryptography, which is used by PasRISCV for various purposes
- And more at [here](https://github.com/Bero1985)
 
### License

This project is licensed under the zlib License - see the [LICENSE](LICENSE) file for details.
