core.sv
    sleep_unit
        cv32e40p_clock_gate(don`t care)

    ifstage
        cv32e40p_prefetch_buffer
            cv32e40p_prefetch_controller
            cv32e40p_fifo
            cv32e40p_obi_interface
        cv32e40p_aligner
        cv32e40p_compressed_decoder

    idstage
        cv32e40p_register_file
            ff
            latch(don`t care)
        cv32e40p_decoder
        cv32e40p_controller
        cv32e40p_int_controller

    exstage
        cv32e40p_alu
            cv32e40p_popcnt
            cv32e40p_ff_one
            cv32e40p_alu_div
        cv32e40p_mult
        cv32e40p_apu_disp

    load_store_unit
        cv32e40p_obi_interface

    cs_registers
    pmp

    /////
    cv32e40p_popcnt
