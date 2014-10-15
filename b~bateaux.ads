pragma Ada_95;
with System;
package ada_main is
   pragma Warnings (Off);

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2014 (20140331)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_bateaux" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#8276a394#;
   pragma Export (C, u00001, "bateauxB");
   u00002 : constant Version_32 := 16#fbff4c67#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#5c291747#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#e18a47a0#;
   pragma Export (C, u00005, "ada__float_text_ioB");
   u00006 : constant Version_32 := 16#e61b3c6c#;
   pragma Export (C, u00006, "ada__float_text_ioS");
   u00007 : constant Version_32 := 16#eaff8cdc#;
   pragma Export (C, u00007, "ada__exceptionsB");
   u00008 : constant Version_32 := 16#6a2091f5#;
   pragma Export (C, u00008, "ada__exceptionsS");
   u00009 : constant Version_32 := 16#032105bb#;
   pragma Export (C, u00009, "ada__exceptions__last_chance_handlerB");
   u00010 : constant Version_32 := 16#2b293877#;
   pragma Export (C, u00010, "ada__exceptions__last_chance_handlerS");
   u00011 : constant Version_32 := 16#5fc8ae56#;
   pragma Export (C, u00011, "systemS");
   u00012 : constant Version_32 := 16#daf76b33#;
   pragma Export (C, u00012, "system__soft_linksB");
   u00013 : constant Version_32 := 16#1517cb64#;
   pragma Export (C, u00013, "system__soft_linksS");
   u00014 : constant Version_32 := 16#c8ed38da#;
   pragma Export (C, u00014, "system__parametersB");
   u00015 : constant Version_32 := 16#591236e4#;
   pragma Export (C, u00015, "system__parametersS");
   u00016 : constant Version_32 := 16#c96bf39e#;
   pragma Export (C, u00016, "system__secondary_stackB");
   u00017 : constant Version_32 := 16#f4a9613f#;
   pragma Export (C, u00017, "system__secondary_stackS");
   u00018 : constant Version_32 := 16#39a03df9#;
   pragma Export (C, u00018, "system__storage_elementsB");
   u00019 : constant Version_32 := 16#720be452#;
   pragma Export (C, u00019, "system__storage_elementsS");
   u00020 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00020, "system__stack_checkingB");
   u00021 : constant Version_32 := 16#d177c5be#;
   pragma Export (C, u00021, "system__stack_checkingS");
   u00022 : constant Version_32 := 16#393398c1#;
   pragma Export (C, u00022, "system__exception_tableB");
   u00023 : constant Version_32 := 16#f1d1c843#;
   pragma Export (C, u00023, "system__exception_tableS");
   u00024 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00024, "system__exceptionsB");
   u00025 : constant Version_32 := 16#37abc3a0#;
   pragma Export (C, u00025, "system__exceptionsS");
   u00026 : constant Version_32 := 16#2652ec14#;
   pragma Export (C, u00026, "system__exceptions__machineS");
   u00027 : constant Version_32 := 16#b895431d#;
   pragma Export (C, u00027, "system__exceptions_debugB");
   u00028 : constant Version_32 := 16#ec2ab7e8#;
   pragma Export (C, u00028, "system__exceptions_debugS");
   u00029 : constant Version_32 := 16#570325c8#;
   pragma Export (C, u00029, "system__img_intB");
   u00030 : constant Version_32 := 16#5d134e94#;
   pragma Export (C, u00030, "system__img_intS");
   u00031 : constant Version_32 := 16#ff5c7695#;
   pragma Export (C, u00031, "system__tracebackB");
   u00032 : constant Version_32 := 16#77cc310b#;
   pragma Export (C, u00032, "system__tracebackS");
   u00033 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00033, "system__wch_conB");
   u00034 : constant Version_32 := 16#44b58c84#;
   pragma Export (C, u00034, "system__wch_conS");
   u00035 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00035, "system__wch_stwB");
   u00036 : constant Version_32 := 16#69a4a085#;
   pragma Export (C, u00036, "system__wch_stwS");
   u00037 : constant Version_32 := 16#9b29844d#;
   pragma Export (C, u00037, "system__wch_cnvB");
   u00038 : constant Version_32 := 16#4b023677#;
   pragma Export (C, u00038, "system__wch_cnvS");
   u00039 : constant Version_32 := 16#69adb1b9#;
   pragma Export (C, u00039, "interfacesS");
   u00040 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00040, "system__wch_jisB");
   u00041 : constant Version_32 := 16#cb722f56#;
   pragma Export (C, u00041, "system__wch_jisS");
   u00042 : constant Version_32 := 16#8cb17bcd#;
   pragma Export (C, u00042, "system__traceback_entriesB");
   u00043 : constant Version_32 := 16#ead9cec4#;
   pragma Export (C, u00043, "system__traceback_entriesS");
   u00044 : constant Version_32 := 16#1ac8b3b4#;
   pragma Export (C, u00044, "ada__text_ioB");
   u00045 : constant Version_32 := 16#ba9eea88#;
   pragma Export (C, u00045, "ada__text_ioS");
   u00046 : constant Version_32 := 16#1b5643e2#;
   pragma Export (C, u00046, "ada__streamsB");
   u00047 : constant Version_32 := 16#2564c958#;
   pragma Export (C, u00047, "ada__streamsS");
   u00048 : constant Version_32 := 16#db5c917c#;
   pragma Export (C, u00048, "ada__io_exceptionsS");
   u00049 : constant Version_32 := 16#034d7998#;
   pragma Export (C, u00049, "ada__tagsB");
   u00050 : constant Version_32 := 16#ce72c228#;
   pragma Export (C, u00050, "ada__tagsS");
   u00051 : constant Version_32 := 16#c3335bfd#;
   pragma Export (C, u00051, "system__htableB");
   u00052 : constant Version_32 := 16#db0a1dbc#;
   pragma Export (C, u00052, "system__htableS");
   u00053 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00053, "system__string_hashB");
   u00054 : constant Version_32 := 16#795476c2#;
   pragma Export (C, u00054, "system__string_hashS");
   u00055 : constant Version_32 := 16#0ece3cf9#;
   pragma Export (C, u00055, "system__unsigned_typesS");
   u00056 : constant Version_32 := 16#4266b2a8#;
   pragma Export (C, u00056, "system__val_unsB");
   u00057 : constant Version_32 := 16#1e66d1c2#;
   pragma Export (C, u00057, "system__val_unsS");
   u00058 : constant Version_32 := 16#27b600b2#;
   pragma Export (C, u00058, "system__val_utilB");
   u00059 : constant Version_32 := 16#f36818a8#;
   pragma Export (C, u00059, "system__val_utilS");
   u00060 : constant Version_32 := 16#d1060688#;
   pragma Export (C, u00060, "system__case_utilB");
   u00061 : constant Version_32 := 16#7bc1c781#;
   pragma Export (C, u00061, "system__case_utilS");
   u00062 : constant Version_32 := 16#9f23726e#;
   pragma Export (C, u00062, "interfaces__c_streamsB");
   u00063 : constant Version_32 := 16#bb1012c3#;
   pragma Export (C, u00063, "interfaces__c_streamsS");
   u00064 : constant Version_32 := 16#75131373#;
   pragma Export (C, u00064, "system__crtlS");
   u00065 : constant Version_32 := 16#967994fc#;
   pragma Export (C, u00065, "system__file_ioB");
   u00066 : constant Version_32 := 16#e3384250#;
   pragma Export (C, u00066, "system__file_ioS");
   u00067 : constant Version_32 := 16#b7ab275c#;
   pragma Export (C, u00067, "ada__finalizationB");
   u00068 : constant Version_32 := 16#19f764ca#;
   pragma Export (C, u00068, "ada__finalizationS");
   u00069 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00069, "system__finalization_rootB");
   u00070 : constant Version_32 := 16#103addc6#;
   pragma Export (C, u00070, "system__finalization_rootS");
   u00071 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00071, "interfaces__cB");
   u00072 : constant Version_32 := 16#3b563890#;
   pragma Export (C, u00072, "interfaces__cS");
   u00073 : constant Version_32 := 16#d0432c8d#;
   pragma Export (C, u00073, "system__img_enum_newB");
   u00074 : constant Version_32 := 16#3e84a896#;
   pragma Export (C, u00074, "system__img_enum_newS");
   u00075 : constant Version_32 := 16#a25be73b#;
   pragma Export (C, u00075, "system__os_libB");
   u00076 : constant Version_32 := 16#94c13856#;
   pragma Export (C, u00076, "system__os_libS");
   u00077 : constant Version_32 := 16#1a817b8e#;
   pragma Export (C, u00077, "system__stringsB");
   u00078 : constant Version_32 := 16#2177bf30#;
   pragma Export (C, u00078, "system__stringsS");
   u00079 : constant Version_32 := 16#906f0f88#;
   pragma Export (C, u00079, "system__file_control_blockS");
   u00080 : constant Version_32 := 16#a4371844#;
   pragma Export (C, u00080, "system__finalization_mastersB");
   u00081 : constant Version_32 := 16#2bde8716#;
   pragma Export (C, u00081, "system__finalization_mastersS");
   u00082 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00082, "system__address_imageB");
   u00083 : constant Version_32 := 16#fe24336c#;
   pragma Export (C, u00083, "system__address_imageS");
   u00084 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00084, "system__img_boolB");
   u00085 : constant Version_32 := 16#aa11dfbd#;
   pragma Export (C, u00085, "system__img_boolS");
   u00086 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00086, "system__ioB");
   u00087 : constant Version_32 := 16#c18a5919#;
   pragma Export (C, u00087, "system__ioS");
   u00088 : constant Version_32 := 16#6d4d969a#;
   pragma Export (C, u00088, "system__storage_poolsB");
   u00089 : constant Version_32 := 16#aa9329d2#;
   pragma Export (C, u00089, "system__storage_poolsS");
   u00090 : constant Version_32 := 16#e34550ca#;
   pragma Export (C, u00090, "system__pool_globalB");
   u00091 : constant Version_32 := 16#c88d2d16#;
   pragma Export (C, u00091, "system__pool_globalS");
   u00092 : constant Version_32 := 16#3a4ba6c3#;
   pragma Export (C, u00092, "system__memoryB");
   u00093 : constant Version_32 := 16#06b5c862#;
   pragma Export (C, u00093, "system__memoryS");
   u00094 : constant Version_32 := 16#7b002481#;
   pragma Export (C, u00094, "system__storage_pools__subpoolsB");
   u00095 : constant Version_32 := 16#e3b008dc#;
   pragma Export (C, u00095, "system__storage_pools__subpoolsS");
   u00096 : constant Version_32 := 16#63f11652#;
   pragma Export (C, u00096, "system__storage_pools__subpools__finalizationB");
   u00097 : constant Version_32 := 16#fe2f4b3a#;
   pragma Export (C, u00097, "system__storage_pools__subpools__finalizationS");
   u00098 : constant Version_32 := 16#d5f9759f#;
   pragma Export (C, u00098, "ada__text_io__float_auxB");
   u00099 : constant Version_32 := 16#f854caf5#;
   pragma Export (C, u00099, "ada__text_io__float_auxS");
   u00100 : constant Version_32 := 16#e0da2b08#;
   pragma Export (C, u00100, "ada__text_io__generic_auxB");
   u00101 : constant Version_32 := 16#a6c327d3#;
   pragma Export (C, u00101, "ada__text_io__generic_auxS");
   u00102 : constant Version_32 := 16#56e74f1a#;
   pragma Export (C, u00102, "system__img_realB");
   u00103 : constant Version_32 := 16#9860ffb4#;
   pragma Export (C, u00103, "system__img_realS");
   u00104 : constant Version_32 := 16#80f37066#;
   pragma Export (C, u00104, "system__fat_llfS");
   u00105 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00105, "system__float_controlB");
   u00106 : constant Version_32 := 16#bf34ed6a#;
   pragma Export (C, u00106, "system__float_controlS");
   u00107 : constant Version_32 := 16#3da6be5a#;
   pragma Export (C, u00107, "system__img_lluB");
   u00108 : constant Version_32 := 16#47073c3a#;
   pragma Export (C, u00108, "system__img_lluS");
   u00109 : constant Version_32 := 16#22ab03a2#;
   pragma Export (C, u00109, "system__img_unsB");
   u00110 : constant Version_32 := 16#913a000e#;
   pragma Export (C, u00110, "system__img_unsS");
   u00111 : constant Version_32 := 16#0fb8c821#;
   pragma Export (C, u00111, "system__powten_tableS");
   u00112 : constant Version_32 := 16#8ff77155#;
   pragma Export (C, u00112, "system__val_realB");
   u00113 : constant Version_32 := 16#a1e1d947#;
   pragma Export (C, u00113, "system__val_realS");
   u00114 : constant Version_32 := 16#0be1b996#;
   pragma Export (C, u00114, "system__exn_llfB");
   u00115 : constant Version_32 := 16#de4cb0b9#;
   pragma Export (C, u00115, "system__exn_llfS");
   u00116 : constant Version_32 := 16#dc11d781#;
   pragma Export (C, u00116, "system__fat_fltS");
   u00117 : constant Version_32 := 16#f64b89a4#;
   pragma Export (C, u00117, "ada__integer_text_ioB");
   u00118 : constant Version_32 := 16#f1daf268#;
   pragma Export (C, u00118, "ada__integer_text_ioS");
   u00119 : constant Version_32 := 16#f6fdca1c#;
   pragma Export (C, u00119, "ada__text_io__integer_auxB");
   u00120 : constant Version_32 := 16#b9793d30#;
   pragma Export (C, u00120, "ada__text_io__integer_auxS");
   u00121 : constant Version_32 := 16#d48b4eeb#;
   pragma Export (C, u00121, "system__img_biuB");
   u00122 : constant Version_32 := 16#c8ecb4b4#;
   pragma Export (C, u00122, "system__img_biuS");
   u00123 : constant Version_32 := 16#2b864520#;
   pragma Export (C, u00123, "system__img_llbB");
   u00124 : constant Version_32 := 16#892ba44a#;
   pragma Export (C, u00124, "system__img_llbS");
   u00125 : constant Version_32 := 16#9777733a#;
   pragma Export (C, u00125, "system__img_lliB");
   u00126 : constant Version_32 := 16#4e87fb87#;
   pragma Export (C, u00126, "system__img_lliS");
   u00127 : constant Version_32 := 16#c2d63ebb#;
   pragma Export (C, u00127, "system__img_llwB");
   u00128 : constant Version_32 := 16#204787dc#;
   pragma Export (C, u00128, "system__img_llwS");
   u00129 : constant Version_32 := 16#8ed53197#;
   pragma Export (C, u00129, "system__img_wiuB");
   u00130 : constant Version_32 := 16#a6ad3326#;
   pragma Export (C, u00130, "system__img_wiuS");
   u00131 : constant Version_32 := 16#f8f38c17#;
   pragma Export (C, u00131, "system__val_intB");
   u00132 : constant Version_32 := 16#176d8469#;
   pragma Export (C, u00132, "system__val_intS");
   u00133 : constant Version_32 := 16#e892b88e#;
   pragma Export (C, u00133, "system__val_lliB");
   u00134 : constant Version_32 := 16#c5ec48f6#;
   pragma Export (C, u00134, "system__val_lliS");
   u00135 : constant Version_32 := 16#1e25d3f1#;
   pragma Export (C, u00135, "system__val_lluB");
   u00136 : constant Version_32 := 16#743c6b8b#;
   pragma Export (C, u00136, "system__val_lluS");
   u00137 : constant Version_32 := 16#2654da16#;
   pragma Export (C, u00137, "system__byte_swappingS");
   u00138 : constant Version_32 := 16#796b5f0d#;
   pragma Export (C, u00138, "system__sequential_ioB");
   u00139 : constant Version_32 := 16#bea7cc55#;
   pragma Export (C, u00139, "system__sequential_ioS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  system%s
   --  system.byte_swapping%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.htable%s
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.img_real%s
   --  system.io%s
   --  system.io%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.powten_table%s
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.os_lib%s
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.soft_links%s
   --  system.unsigned_types%s
   --  system.fat_flt%s
   --  system.fat_llf%s
   --  system.img_biu%s
   --  system.img_biu%b
   --  system.img_llb%s
   --  system.img_llb%b
   --  system.img_llu%s
   --  system.img_llu%b
   --  system.img_llw%s
   --  system.img_llw%b
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.img_real%b
   --  system.img_wiu%s
   --  system.img_wiu%b
   --  system.val_int%s
   --  system.val_lli%s
   --  system.val_llu%s
   --  system.val_real%s
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
   --  system.val_real%b
   --  system.val_llu%b
   --  system.val_lli%b
   --  system.val_int%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  system.address_image%s
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.io_exceptions%s
   --  ada.tags%s
   --  ada.streams%s
   --  ada.streams%b
   --  interfaces.c%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.finalization%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.file_control_block%s
   --  system.file_io%s
   --  system.secondary_stack%s
   --  system.file_io%b
   --  system.storage_pools.subpools%b
   --  system.finalization_masters%b
   --  interfaces.c%b
   --  ada.tags%b
   --  system.soft_links%b
   --  system.os_lib%b
   --  system.secondary_stack%b
   --  system.address_image%b
   --  system.sequential_io%s
   --  system.sequential_io%b
   --  system.traceback%s
   --  ada.exceptions%b
   --  system.traceback%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  ada.text_io.float_aux%s
   --  ada.float_text_io%s
   --  ada.float_text_io%b
   --  ada.text_io.generic_aux%s
   --  ada.text_io.generic_aux%b
   --  ada.text_io.float_aux%b
   --  ada.text_io.integer_aux%s
   --  ada.text_io.integer_aux%b
   --  ada.integer_text_io%s
   --  ada.integer_text_io%b
   --  bateaux%b
   --  END ELABORATION ORDER


end ada_main;