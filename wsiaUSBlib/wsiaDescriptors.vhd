--wsiaDescriptors
--a library required by wsiaUSB DE2 ISP1362 USB firmware by Tony Slagle

library ieee, wsiaUSBlib;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
--use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
use wsiaUSBlib.wsiaUseful.all;

package wsiaDescriptors is 
--=-=-=-=-=-TYPE DECLARATIONS-=-=-=-=-=--
type deviceDescriptor is
record
	bLength			: byte;--:=x"12";
	bDescriptorType	: byte;--:=x"01";
	bcdUSB			: word;--:=x"0200";
	bDeviceClass	: byte;--:=x"FF";
	bDeviceSubClass	: byte;--:=x"FF";
	bDeviceProtocol	: byte;--:=x"FF";
	bMaxPacketSize0	: byte;--:=x"40";
	idVendor		: word;--:=x"7777";
	idProduct		: word;--:=x"7777";
	bcdDevice		: word;--:=x"0010";
	iManufacturer	: byte;--:=x"01";
	iProduct		: byte;--:=x"02";
	iSerialNumber	: byte;--:=x"00";
	bNumConfigs		: byte;--:=x"01";
end record;
type configurationDescriptor is
record
	bLength			: byte;--:=x"09";
	bDescriptorType	: byte;--:=x"02";
	wTotalLength	: word;--:=std_logic_vector(unsigned(9+));			--FIXMEFIXMEFIXME
	bNumInterfaces	: byte;--:=x"02";
	bConfigValue	: byte;--:=x"01";
	iConfiguration	: byte;--:=x"00";
	bmAttributes	: byte;--:=x"C0";
	bMaxPower		: byte;--:=x"00";
end record;
type interfaceDescriptor is
record
	bLength				: byte;--:=x"09";
	bDescriptorType		: byte;--:=x"04";
	bInterfaceNumber	: byte;--:=x"00";
	bAlternateSetting	: byte;--;
	bNumEndpoints		: byte;--;
	bInterfaceClass		: byte;--:=x"FF";
	bInterfaceSubClass	: byte;--:=x"FF";
	bInterfaceProtocol	: byte;--:=x"FF";
	iInterface			: byte;--:=x"00";
end record;
type endpointDescriptor is
record
	bLength			: byte;--:=x"07";
	bDescriptorType	: byte;--:=x"05";
	bEndpointAddress: byte;
	bmAttributes	: byte;
	wMaxPacketSize	: word;
	bInterval		: byte;--:=x"01";
end record;



--constant assembled_configuration_descriptor	: std_logic_vector(0 to 8*(9 + 2*9 + 5*7)-1):=dizzy_indian(cfgDesc & intDesc1 & intDesc2 & ep1Desc & ep2Desc & ep3Desc & ep4Desc & ep5Desc);
--=-=-=-=-=-DESCRIPTOR TYPES-=-=-=-=-=--
constant desc_DEVICE			: byte	:= x"01";
constant desc_CONFIGURATION		: byte	:= x"02";
constant desc_STRING			: byte	:= x"03";
constant desc_INTERFACE			: byte	:= x"04";
constant desc_ENDPOINT			: byte	:= x"05";
constant desc_DEVICE_QUALIFIER	: byte	:= x"06";
constant desc_OTHER_SPEED_CFG	: byte	:= x"07";
constant desc_INTERFACE_POWER	: byte	:= x"08";


constant CRD_devDesc	: deviceDescriptor:=(bLength		=> x"12",
											bDescriptorType	=> desc_DEVICE,
											bcdUSB			=> x"0200",
											bDeviceClass	=> x"FF",
											bDeviceSubClass	=> x"FF",
											bDeviceProtocol	=> x"FF",
											bMaxPacketSize0	=> x"40",
											idVendor		=> x"7777",
											idProduct		=> x"7777",
											bcdDevice		=> x"0091",
											iManufacturer	=> x"01",
											iProduct		=> x"02",
											iSerialNumber	=> x"00",
											bNumConfigs		=>x"01");
constant CRD_strDesc_00_Langs	: std_logic_vector(0 to (2+2)*8-1)	:=x"04030904";
constant CRD_strDesc_01_Vendor	: std_logic_vector(0 to (36+2)*8-1)	:=x"260354006F006E007900200053006C00610067006C0065002000610074002000410053005500";
constant CRD_strDesc_02_Product	: std_logic_vector(0 to (34+2)*8-1)	:=x"240350004F00530020005500530042002000430052004400200042006F00610072006400";
constant CRD_strDesc_03_Serial	: std_logic_vector(0 to (44+2)*8-1) :=x"2E0300430065007200650061006C0020004E006F00200042003400550032006500610074004E0052006C00610062";
--constant CRD_strDesc_: std_logic_vector(0 to (decimalLength+2)*8-1):=x"hexLength03

constant CRD_cfg1Desc	: configurationDescriptor:=(bLength			=>x"09",
													bDescriptorType	=>x"02",
													wTotalLength	=>to_vec(16,(9+9+7*2)),
													bNumInterfaces	=>x"01",
													bConfigValue	=>x"01",
													iConfiguration	=>x"00",
													bmAttributes	=>x"C0",
													bMaxPower		=>x"32");
constant CRD_cfg2Desc	: configurationDescriptor:=(bLength			=>x"09",
													bDescriptorType	=>x"02",
													wTotalLength	=>to_vec(16,(9+9+7*5)),
													bNumInterfaces	=>x"01",
													bConfigValue	=>x"02",
													iConfiguration	=>x"00",
													bmAttributes	=>x"C0",
													bMaxPower		=>x"32");
constant CRD_cfgDesc	: configurationDescriptor:=(bLength			=>x"09",
													bDescriptorType	=>x"02",
													wTotalLength	=>to_vec(16,(9+9+9+7*5)),
													bNumInterfaces	=>x"02",
													bConfigValue	=>x"01",
													iConfiguration	=>x"00",
													bmAttributes	=>x"C0",
													bMaxPower		=>x"32");
constant CRD_int1Desc	: interfaceDescriptor:=(bLength				=>x"09",
												bDescriptorType		=>x"04",
												bInterfaceNumber	=>x"00",
												bAlternateSetting	=>x"00",
												bNumEndpoints		=>x"02",
												bInterfaceClass		=>x"FF",
												bInterfaceSubClass	=>x"FF",
												bInterfaceProtocol	=>x"FF",
												iInterface			=>x"00");
constant CRD_int2Desc	: interfaceDescriptor:=(bLength				=>x"09",
												bDescriptorType		=>x"04",
												bInterfaceNumber	=>x"00",
												bAlternateSetting	=>x"01",
												bNumEndpoints		=>x"05",
												bInterfaceClass		=>x"FF",
												bInterfaceSubClass	=>x"FF",
												bInterfaceProtocol	=>x"FF",
												iInterface			=>x"00");

constant CRD_endp1Desc	: endpointDescriptor:=(	bLength			=>x"07",
												bDescriptorType	=>x"05",
												bEndpointAddress=>x"01",
												bmAttributes	=>x"02",
												wMaxPacketSize	=>x"0040",
												bInterval		=>x"01");
constant CRD_endp2Desc	: endpointDescriptor:=(	bLength			=>x"07",
												bDescriptorType	=>x"05",
												bEndpointAddress=>x"82",
												bmAttributes	=>x"02",
												wMaxPacketSize	=>x"0040",
												bInterval		=>x"01");
constant CRD_endp3Desc	: endpointDescriptor:=(	bLength			=>x"07",
												bDescriptorType	=>x"05",
												bEndpointAddress=>x"03",
												bmAttributes	=>x"03",
												wMaxPacketSize	=>x"0010",
												bInterval		=>x"01");
constant CRD_endp4Desc	: endpointDescriptor:=(	bLength			=>x"07",
												bDescriptorType	=>x"05",
												bEndpointAddress=>x"84",
												bmAttributes	=>x"03",
												wMaxPacketSize	=>x"0010",
												bInterval		=>x"01");
constant CRD_endp5Desc	: endpointDescriptor:=(	bLength			=>x"07",
												bDescriptorType	=>x"05",
												bEndpointAddress=>x"85",
												bmAttributes	=>x"01",
												wMaxPacketSize	=>x"07FF",
												bInterval		=>x"01");

--not a descriptor, is the endpoint configuration bytes for DcEndpointConfiguration registers
constant DcEndpointConfiguration: byte16:= ("10000011",--ctrlOut
											"11000011",--ctrlIn
											"10000011",--64b Bulk Out
											"11000011",--64b Bulk In
											"10000001",--16b Int Out
											"11000001",--16b Int In
											"11111111",--1023b Iso In (dblBuff)
											"00000000","00000000","00000000","00000000","00000000","00000000","00000000","00000000","00000000");


function byte_deviceDescriptor( constant descrip: in deviceDescriptor ) return std_logic_vector;
function byte_configurationDescriptor( constant descrip: in configurationDescriptor ) return std_logic_vector;
function byte_interfaceDescriptor( constant descrip: in interfaceDescriptor ) return std_logic_vector;
function byte_endpointDescriptor( constant descrip: in endpointDescriptor ) return std_logic_vector;

constant CRD_Full_Cfg1_Desc:std_logic_vector(1 to 8*(9+9+7*2)):=(
							byte_configurationDescriptor(CRD_cfg1Desc) & 
							byte_interfaceDescriptor(CRD_int1Desc) & 
							byte_endpointDescriptor(CRD_endp1Desc) & 
							byte_endpointDescriptor(CRD_endp2Desc));
constant CRD_Full_Cfg2_Desc:std_logic_vector(1 to 8*(9+9+7*5)):=(
							byte_configurationDescriptor(CRD_cfg2Desc) & 
							byte_interfaceDescriptor(CRD_int2Desc) & 
							byte_endpointDescriptor(CRD_endp1Desc) & 
							byte_endpointDescriptor(CRD_endp2Desc) & 
							byte_endpointDescriptor(CRD_endp3Desc) & 
							byte_endpointDescriptor(CRD_endp4Desc) & 
							byte_endpointDescriptor(CRD_endp5Desc));
constant CRD_Full_Cfg_Desc:std_logic_vector(1 to 8*(9+9+9+7*5)):=(
							byte_configurationDescriptor(CRD_cfgDesc) & 
							byte_interfaceDescriptor(CRD_int1Desc) & 
							byte_interfaceDescriptor(CRD_int2Desc) & 
							byte_endpointDescriptor(CRD_endp1Desc) & 
							byte_endpointDescriptor(CRD_endp2Desc) & 
							byte_endpointDescriptor(CRD_endp3Desc) & 
							byte_endpointDescriptor(CRD_endp4Desc) & 
							byte_endpointDescriptor(CRD_endp5Desc));

end wsiaDescriptors; 


package body wsiaDescriptors is 
function byte_deviceDescriptor( constant d: in deviceDescriptor ) return std_logic_vector is
begin
	return(	d.bLength & 
			d.bDescriptorType & 
			d.bcdUSB(7 downto 0) & 
			d.bcdUSB(15 downto 8) & 
			d.bDeviceClass & 
			d.bDeviceSubClass & 
			d.bDeviceProtocol & 
			d.bMaxPacketSize0 & 
			d.idVendor(7 downto 0) & 
			d.idVendor(15 downto 8) & 
			d.idProduct(7 downto 0) & 
			d.idProduct(15 downto 8) & 
			d.bcdDevice(7 downto 0) & 
			d.bcdDevice(15 downto 8) & 
			d.iManufacturer & 
			d.iProduct & 
			d.iSerialNumber & 
			d.bNumConfigs);
end function byte_deviceDescriptor;

function byte_configurationDescriptor( constant d: in configurationDescriptor ) return std_logic_vector is
begin
	return(	d.bLength & 
			d.bDescriptorType & 
			d.wTotalLength(7 downto 0) & 
			d.wTotalLength(15 downto 8) & 
			d.bNumInterfaces & 
			d.bConfigValue & 
			d.iConfiguration & 
			d.bmAttributes & 
			d.bMaxPower);
end function byte_configurationDescriptor;

function byte_interfaceDescriptor( constant d: in interfaceDescriptor ) return std_logic_vector is
begin
	return(	d.bLength & 
			d.bDescriptorType & 
			d.bInterfaceNumber & 
			d.bAlternateSetting & 
			d.bNumEndpoints & 
			d.bInterfaceClass & 
			d.bInterfaceSubClass & 
			d.bInterfaceProtocol & 
			d.iInterface);
end function byte_interfaceDescriptor;

function byte_endpointDescriptor( constant d: in endpointDescriptor ) return std_logic_vector is
begin
	return(	d.bLength & 
			d.bDescriptorType & 
			d.bEndpointAddress & 
			d.bmAttributes & 
			d.wMaxPacketSize(7 downto 0) & 
			d.wMaxPacketSize(15 downto 8) & 
			d.bInterval);
end function byte_endpointDescriptor;

	
end wsiaDescriptors;