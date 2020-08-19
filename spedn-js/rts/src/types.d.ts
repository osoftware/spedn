declare module "varuint-bitcoin" {
  interface IEncode {
    (_number: number, buffer: Buffer, offset: number): Buffer;
    bytes: number;
  }
  const encode: IEncode;
  function decode(buffer: Buffer, offset: number): number;
  function encodingLength(_number: number): number;
}
