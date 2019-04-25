# Images

(b = byte; bi = bit)

- magic: 8b (0x039963)
- symbol-table:
  - length: 8b
    - strings: length * string
      - string.length: 8b
      - string.bytes: length * 1b
- const-area:
  - length: 8b
    - values: length * value
      - value: ?
- global-method: method

## MIR.images

- line: 32bi
- opcode: 1b
- value: 8b

### MIR.rawMir (`core` and `std` runtime)

- load_const            `<module-constant-area>`
- load_lambda           `<module-method-area>`
- load_symbol           `<symbol-table-offset>`
- write_local_symbol    `<symbol-table-offset>`
- write_object_symbol   `<symbol-table-offset>`

- get_method            `<symbol-table-offset>`

- copy_to_shared_area
- delete
- gc_new

- call                  `<symbol-table-offset>`
- return

### MIR.optimizMir

- load_local_of_offset      `<uint64>`
- load_object_of_offset     `<uint64>`
- write_local_of_offset     `<uint64>`
- write_object_of_offset    `<uint64>`

- get_method_of_offset      `<uint64>`

- call_of_offset            `<uint64>`

### MIR.optimizL2Mir

- throw
- add   `<uint32>` `<uint32>`
- sub   `<uint32>` `<uint32>`
- mul   `<uint32>` `<uint32>`
- div   `<uint32>` `<uint32>`
- mod   `<uint32>` `<uint32>`
