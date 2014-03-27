--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
--=	Wonderfully Simple ISP1362 Altera DE2 Interface Thing					=-
--=	VERSION 0.1  -- data can be sent from computer to board					=-
--=																			=-
--= ...simple description goes here... after I figure out what this thing is going to do
--=
--=	I'm currently too pressed for time to make this officially public domain=-
--= or open licence but that will happen.  I can't stop you from stealing my=-
--= work and claiming it as your own, but if you do, try and remember me	=-
--= when the boss says you're looking to hire.  Some credit	and an email	=-
--= wouldn't hurt if you find this useful for any sort of official project.	=-
--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

library ieee, wsiaUSBlib, wsiaDescriptors;
use ieee.std_logic_1164.all;
--use ieee.std_logic_arith.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
use wsiaUSBlib.wsiaUseful.all;
use wsiaUSBlib.wsiaDescriptors.all;

entity bus_as_cpu is
port(
	CLOCK_50	: in	std_logic;
--	reset_n		: in	std_logic;
--	instruction	: in	worker_states;
--	w_data32	: in	word32;
--	w_length	: in	word;
--	w_endpoint	: in	std_logic_vector(3 downto 0);
--	w_execute	: in	std_logic;	--'1' => start executing instruction.
--	w_done		: out	std_logic;
--	w_ctrl_xfer	: out	byte16;
--registers
--	wi_DcAddress				: in		byte;
--	wi_DcMode					: in		byte;
--	wi_DcHardwareConfiguration	: in		word;
--	wi_DcEndpointConfiguration	: in		word;
--	wi_DcInterruptEnable		: in		dWord;
--	wo_DcAddress				: buffer	byte;
--	wo_DcMode					: buffer	byte;
--	wo_DcHardwareConfiguration	: buffer	word;
--	wo_DcInterruptEnable		: buffer	dWord;
--	wo_DcInterrupt				: buffer	dWord;
--	wo_ESR						: out		byte;
	OTG_INT1	: in		std_logic;						--ISP1362 Interrupt 2 (Peripheral Interrupts) 
	OTG_DATA	: inout		std_logic_vector(15 downto 0);	--ISP1362 Data bus 16 bits
	OTG_RST_N	: out		std_logic;						--ISP1362 Reset pin
	OTG_ADDR	: out		std_logic_vector(1 downto 0);	--ISP1362 Address 2 Bits[peripheral,command]
	OTG_CS_N	: out		std_logic;						--ISP1362 Chip Select 
	OTG_RD_N	: out		std_logic;						--ISP1362 Write 
	OTG_WR_N	: out		std_logic;						--ISP1362 Read 

--IGNORE/SET AND FORGET
	OTG_FSPEED	: out		std_logic:='0';					--USB Full Speed,  0 = Enable, Z = Disable 
	OTG_LSPEED	: out		std_logic:='Z';					--USB Low Speed,   0 = Enable, Z = Disable 
	OTG_INT0	: in		std_logic;						--ISP1362 Interrupt 1 (Host Interrupts)
	OTG_DREQ0	: in		std_logic;						--ISP1362 DMA Request 1
	OTG_DREQ1	: in		std_logic;						--ISP1362 DMA Request 2
	OTG_DACK0_N	: out		std_logic:='1';					--ISP1362 DMA Acknowledge 1 
	OTG_DACK1_N	: out		std_logic:='1';					--ISP1362 DMA Acknowledge 2

--DIAGNOSTICS STUFF...
	KEY										: in	std_logic_vector(3 downto 0);
	SW										: in	std_logic_vector(17 downto 0);
	LEDR									: out	std_logic_vector(17 downto 0);
	LEDG									: out	std_logic_vector(8 downto 0);
	HEX0,HEX1,HEX2,HEX3,HEX4,HEX5,HEX6,HEX7	: out	std_logic_vector(0 to 6)
	);
end bus_as_cpu;

architecture handler of bus_as_cpu is
component SevenSeg is port(
	inNum	:in std_logic_vector(3 downto 0);
	outSeg	:out std_logic_vector(6 downto 0));
end component SevenSeg;
signal w_count		: integer;
signal segs1		: std_logic_vector(15 downto 0);
signal segs2, segs3	: std_logic_vector(7 downto 0);
signal CLOCK_25		: std_logic;
signal cur_instruction		: worker_states;
signal stack				: word32;
signal SENT_DATA			: word32;
signal IP, IP_JMP			: word		:= x"0000";
signal JMP					: std_logic	:= 'L';	--Weak 0
signal SP, SP_NEXT			: unsigned(4 downto 0)	:= "00000";
signal EAX,EBX,ECX,EDX,nEAX,nEBX,nECX,nEDX	: dword		:= x"00000000";
alias ax : word is eax(15 downto 0);alias bx : word is ebx(15 downto 0);
alias cx : word is ecx(15 downto 0);alias dx : word is edx(15 downto 0);
alias ah : byte is eax(15 downto 8);alias al : byte is eax(7 downto 0);
alias bh : byte is ebx(15 downto 8);alias bl : byte is ebx(7 downto 0);
alias ch : byte is ecx(15 downto 8);alias cl : byte is ecx(7 downto 0);
alias dh : byte is edx(15 downto 8);alias dl : byte is edx(7 downto 0);
alias nax : word is neax(15 downto 0);alias nbx : word is nebx(15 downto 0);
alias ncx : word is necx(15 downto 0);alias ndx : word is nedx(15 downto 0);
alias nah : byte is neax(15 downto 8);alias nal : byte is neax(7 downto 0);
alias nbh : byte is nebx(15 downto 8);alias nbl : byte is nebx(7 downto 0);
alias nch : byte is necx(15 downto 8);alias ncl : byte is necx(7 downto 0);
alias ndh : byte is nedx(15 downto 8);alias ndl : byte is nedx(7 downto 0);
--registers
signal	OTG_DcAddress				: byte;
signal	OTG_DcMode					: byte;
signal	OTG_DcHardwareConfiguration	: word;
signal	OTG_DcEndpointConfiguration	: word16;
signal	OTG_DcInterruptEnable		: dWord;
signal	OTG_DcInterrupt				: dWord;
signal	OTG_ESR						: byte16;
signal	OTG_INT1_latch				: std_logic;
signal	instruction	: worker_states;
signal	which_interface	: byte:=x"00";
signal	what_config		: byte:=x"00";
signal	w_buffer64	: buffer64;
signal	w_data32	: word32;--w_data32<=(w_buffer64(0*16+8 to 0*16+15) & w_buffer64(0*16 to 0*16+7),w_buffer64(1*16+8 to 1*16+15) & w_buffer64(1*16 to 1*16+7),w_buffer64(2*16+8 to 2*16+15) & w_buffer64(2*16 to 2*16+7),w_buffer64(3*16+8 to 3*16+15) & w_buffer64(3*16 to 3*16+7),w_buffer64(4*16+8 to 4*16+15) & w_buffer64(4*16 to 4*16+7),w_buffer64(5*16+8 to 5*16+15) & w_buffer64(5*16 to 5*16+7),w_buffer64(6*16+8 to 6*16+15) & w_buffer64(6*16 to 6*16+7),w_buffer64(7*16+8 to 7*16+15) & w_buffer64(7*16 to 7*16+7),w_buffer64(8*16+8 to 8*16+15) & w_buffer64(8*16 to 8*16+7),w_buffer64(9*16+8 to 9*16+15) & w_buffer64(9*16 to 9*16+7),w_buffer64(10*16+8 to 10*16+15) & w_buffer64(10*16 to 10*16+7),w_buffer64(11*16+8 to 11*16+15) & w_buffer64(11*16 to 11*16+7),w_buffer64(12*16+8 to 12*16+15) & w_buffer64(12*16 to 12*16+7),w_buffer64(13*16+8 to 13*16+15) & w_buffer64(13*16 to 13*16+7),w_buffer64(14*16+8 to 14*16+15) & w_buffer64(14*16 to 14*16+7),w_buffer64(15*16+8 to 15*16+15) & w_buffer64(15*16 to 15*16+7),w_buffer64(16*16+8 to 16*16+15) & w_buffer64(16*16 to 16*16+7),w_buffer64(17*16+8 to 17*16+15) & w_buffer64(17*16 to 17*16+7),w_buffer64(18*16+8 to 18*16+15) & w_buffer64(18*16 to 18*16+7),w_buffer64(19*16+8 to 19*16+15) & w_buffer64(19*16 to 19*16+7),w_buffer64(20*16+8 to 20*16+15) & w_buffer64(20*16 to 20*16+7),w_buffer64(21*16+8 to 21*16+15) & w_buffer64(21*16 to 21*16+7),w_buffer64(22*16+8 to 22*16+15) & w_buffer64(22*16 to 22*16+7),w_buffer64(23*16+8 to 23*16+15) & w_buffer64(23*16 to 23*16+7),w_buffer64(24*16+8 to 24*16+15) & w_buffer64(24*16 to 24*16+7),w_buffer64(25*16+8 to 25*16+15) & w_buffer64(25*16 to 25*16+7),w_buffer64(26*16+8 to 26*16+15) & w_buffer64(26*16 to 26*16+7),w_buffer64(27*16+8 to 27*16+15) & w_buffer64(27*16 to 27*16+7),w_buffer64(28*16+8 to 28*16+15) & w_buffer64(28*16 to 28*16+7),w_buffer64(29*16+8 to 29*16+15) & w_buffer64(29*16 to 29*16+7),w_buffer64(30*16+8 to 30*16+15) & w_buffer64(30*16 to 30*16+7),w_buffer64(31*16+8 to 31*16+15) & w_buffer64(31*16 to 31*16+7));
signal	w_length	: word;
signal	w_endpoint	: std_logic_vector(3 downto 0);
signal	w_execute	: std_logic;	--'1' => start executing instruction.
signal	w_done		: std_logic;
signal	w_ctrl_xfer	: setup_packet_type;
type	eight_ctrls is array(0 to 7) of setup_packet_type;
signal	raw_ctrl_xfers:eight_ctrls;
signal	e_state		: enum_state;
signal	IP_History	: word32;
signal	IP_H_View	: boolean;
signal	IP_H_V_num	: integer range 0 to 31;
signal	keylast		: std_logic_vector(3 downto 0);
signal EP1_Buffer	: buffer64;--EP1	--64b Bulk Out
signal EP1_Buff	: word32;
signal EP2_Buffer	: buffer64:=x"5400680065002000620075006700670061007200200077006F0072006B0073002100210021002000490020004C00750076002000530061007200610021002100";--EP2	--64b Bulk In
signal EP3_Buffer	: buffer16;--EP3	--16b Int Out
signal EP3_Buff	: word8;
signal EP4_Buffer	: buffer16;--EP4	--16b Int In


constant sub_reset_Dc		: word	:= x"1000";	--resets the device controller of the ISP1362
constant sub_port_out_cmd	: word	:= x"1100";	--writes a command from AX
constant sub_port_out		: word	:= x"1200";	--writes a word from AX
constant sub_port_in		: word	:= x"1300";	--reads a word into AX
constant sub_send_data		: word	:= x"1400";	--endpoint in DL, length in w_length, data in w_data32
constant sub_port_dump		: word	:= x"1500";	--number of bytes in CX, data in w_data32
constant sub_rd_cfg_regs	: word	:= x"1600";	--reads from chip to reg_DcRegisters (mode, hwcfg, intenable only)
constant sub_CRwrite		: word	:= x"1700";	--writes command from AX and corresponding Reg_DcRegister
constant sub_rcv_setup		: word	:= x"1800";	--recieves 8 bytes from ctrlOut to w_ctrl_xfer
constant sub_init_isp1362	: word	:= x"1900";	--initilizes isp1362 configuration DcRegisters
constant sub_disp_cfg_regs	: word	:= x"1A00";	--displays masked Dcmode, DcHardwareConfiguration and last word of DcInterruptEnable
constant sub_DcInterrupt	: word	:= x"1B00";	--loads OTG_DcInterrupt from isp1362
constant sub_suspender		: word	:= x"1C00";	--handles suspend state and wakeup
constant sub_ctrlOut_handler: word	:= x"1D00";	--
constant sub_Get_ESR		: word	:= x"1E00";	--reads ESR specified by command in AX into OTG_ESR register and AL
constant sub_SET_ADDRESS	: word	:= x"1F00";	--handles SET_ADDRESS setup packet
constant sub_configureEps_n_ack:word:= x"2000"; --configures endpoints and acknowledges
constant sub_sendEpStatus	: word	:= x"2200";	--send EpStatus and ack
constant sub_EP_Int_handler	: word	:= x"2300";	--handles endpoint interrupts
--constant sub_	: word	:= x"2200";	--
--constant sub_	: word	:= x"2300";	--

procedure reset_cpu is
begin
	OTG_RST_N	<= '1';
	OTG_ADDR(0)	<= '1';
	OTG_CS_N	<= '1';
	OTG_RD_N	<= '1';
	OTG_WR_N	<= '1';
	nEAX	<= x"00000000";
	nEBX	<= x"00000000";
	nECX	<= x"00000000";
	nEDX	<= x"00000000";
	sp_next	<= "00000";
	w_done	<= '1';
	e_state	<= default;
	w_count	<= 0;
end reset_cpu;

procedure push(constant data : in word) is
begin
	stack(to_integer(SP-1)) <= data;
	SP_NEXT					<= SP-1;
end push;
procedure pop(signal data : out word) is
begin
	data	<= stack(to_integer(sp));
	SP_NEXT	<= SP+1;
end pop;
procedure jump(constant New_IP : in word) is
begin
	JMP		<= '1';
	IP_JMP	<= New_IP;
end jump;
procedure loopJump(constant New_IP : in word) is
begin
	nCX	<= to_vec(16,to_int(CX) - 1);
	if CX /= x"0000" then
		jump(New_IP);
	end if;
end loopJump;
procedure go_sub(constant new_ip : in word) is
begin
	stack(to_integer(SP-1))	<= IP+1;	--we'll return to next ip
	SP_NEXT					<= SP-1;
	JMP						<= '1';
	IP_JMP					<= new_ip;
end go_sub;
procedure ret_sub is
begin
	IP_JMP	<= stack(to_integer(SP));	--ip incremented by go_sub
	JMP		<= '1';
	SP_NEXT	<= SP+1;
end ret_sub;
procedure wait_here(constant num_clocks : in unsigned) is	--wait_here(x"0000") is same as NoOp
begin
	if w_count < num_clocks then
		w_count	<= w_count + 1;
		IP_JMP	<= IP;
		JMP		<= '1';
	else
		w_count	<= 0;
	end if;
end wait_here;
procedure wait_for_command is
begin
	if w_execute = '1' then
		cur_instruction <= instruction;
		w_done			<= '0';
	else
		jump(IP);
	end if;
end wait_for_command;
procedure loadBuffer(constant with_me : in std_logic_vector) is
begin
	w_buffer64(0 to with_me'Length-1)<=with_me;
end loadBuffer;
procedure port_dump(constant destination:in nibble;
					constant to_send	:in std_logic_vector;
					constant length_limit	:in word := x"FFFF") is
begin
	w_length<=smaller(to_vec(16,(to_send'length)/8),length_limit);
	loadBuffer(to_send);
	nDL(3 downto 0)<=destination;
	go_sub(sub_send_data);
end port_dump;

begin
Hexx0	: SevenSeg port map(segs1(3 downto 0),HEX0(0 to 6));
Hexx1	: SevenSeg port map(segs1(7 downto 4),HEX1(0 to 6));
Hexx2	: SevenSeg port map(segs1(11 downto 8),HEX2(0 to 6));
Hexx3	: SevenSeg port map(segs1(15 downto 12),HEX3(0 to 6));
Hexx4	: SevenSeg port map(segs2(3 downto 0),HEX4(0 to 6));
Hexx5	: SevenSeg port map(segs2(7 downto 4),HEX5(0 to 6));
Hexx6	: SevenSeg port map(segs3(3 downto 0),HEX6(0 to 6));
Hexx7	: SevenSeg port map(segs3(7 downto 4),HEX7(0 to 6));

--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-CLOCK PROCESS-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--
clock_halfer: process
begin
	wait until clock_50'EVENT and clock_50='1';
	if clock_25 = '1' then
		clock_25 <= '0';
	else
		clock_25 <= '1';
	end if;
end process;
--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-IP_Mover PROCESS-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--
IP_Mover	: process	--Increments IP on falling clock edge (IP stable for 20ns after clock rise)
begin					--If jmp = true then instead sets IP to IP_JMP
						--ALSO SETS SP = SP_NEXT
						--ALSO SETS EAX,EBX,ECX,EDX to nEAX,nEBX,nECX,nEDX
						--ALSO latches OTG_INT1_latch
						--ALSO translates w_buffer64 into w_data32
	wait until CLOCK_25'EVENT and CLOCK_25 = '0';
	if JMP = '1' then
		IP	<= IP_JMP;
	else
		IP	<= IP+1;
	end if;
	SP	<= SP_NEXT;
	EAX	<= nEAX;
	EBX	<= nEBX;
	ECX	<= nECX;
	EDX	<= nEDX;
	OTG_INT1_latch	<= OTG_INT1;
	w_data32<=(w_buffer64(0*16+8 to 0*16+15) & w_buffer64(0*16 to 0*16+7),				w_buffer64(1*16+8 to 1*16+15) & w_buffer64(1*16 to 1*16+7),				w_buffer64(2*16+8 to 2*16+15) & w_buffer64(2*16 to 2*16+7),				w_buffer64(3*16+8 to 3*16+15) & w_buffer64(3*16 to 3*16+7),				w_buffer64(4*16+8 to 4*16+15) & w_buffer64(4*16 to 4*16+7),				w_buffer64(5*16+8 to 5*16+15) & w_buffer64(5*16 to 5*16+7),				w_buffer64(6*16+8 to 6*16+15) & w_buffer64(6*16 to 6*16+7),				w_buffer64(7*16+8 to 7*16+15) & w_buffer64(7*16 to 7*16+7),				w_buffer64(8*16+8 to 8*16+15) & w_buffer64(8*16 to 8*16+7),				w_buffer64(9*16+8 to 9*16+15) & w_buffer64(9*16 to 9*16+7),				w_buffer64(10*16+8 to 10*16+15) & w_buffer64(10*16 to 10*16+7),w_buffer64(11*16+8 to 11*16+15) & w_buffer64(11*16 to 11*16+7),w_buffer64(12*16+8 to 12*16+15) & w_buffer64(12*16 to 12*16+7),w_buffer64(13*16+8 to 13*16+15) & w_buffer64(13*16 to 13*16+7),w_buffer64(14*16+8 to 14*16+15) & w_buffer64(14*16 to 14*16+7),w_buffer64(15*16+8 to 15*16+15) & w_buffer64(15*16 to 15*16+7),w_buffer64(16*16+8 to 16*16+15) & w_buffer64(16*16 to 16*16+7),w_buffer64(17*16+8 to 17*16+15) & w_buffer64(17*16 to 17*16+7),w_buffer64(18*16+8 to 18*16+15) & w_buffer64(18*16 to 18*16+7),w_buffer64(19*16+8 to 19*16+15) & w_buffer64(19*16 to 19*16+7),w_buffer64(20*16+8 to 20*16+15) & w_buffer64(20*16 to 20*16+7),w_buffer64(21*16+8 to 21*16+15) & w_buffer64(21*16 to 21*16+7),w_buffer64(22*16+8 to 22*16+15) & w_buffer64(22*16 to 22*16+7),w_buffer64(23*16+8 to 23*16+15) & w_buffer64(23*16 to 23*16+7),w_buffer64(24*16+8 to 24*16+15) & w_buffer64(24*16 to 24*16+7),w_buffer64(25*16+8 to 25*16+15) & w_buffer64(25*16 to 25*16+7),w_buffer64(26*16+8 to 26*16+15) & w_buffer64(26*16 to 26*16+7),w_buffer64(27*16+8 to 27*16+15) & w_buffer64(27*16 to 27*16+7),w_buffer64(28*16+8 to 28*16+15) & w_buffer64(28*16 to 28*16+7),w_buffer64(29*16+8 to 29*16+15) & w_buffer64(29*16 to 29*16+7),w_buffer64(30*16+8 to 30*16+15) & w_buffer64(30*16 to 30*16+7),w_buffer64(31*16+8 to 31*16+15) & w_buffer64(31*16 to 31*16+7));
end process;

	OTG_ADDR(1)	<= '1';	--always talking to the peripheral

--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-WORKER PROCESS-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--
	worker	: process
	begin
		wait until clock_25'EVENT and clock_25 = '1';
		JMP	<= 'L';	--Weak Low  If someone else wants to jump, force it with '1';
keylast<=key;
if keylast(3)='0' and key(3)='1' then	--start viewing ip history
	if IP_H_View then
		IP_H_View	<= false;
	else
		IP_H_View	<= true;
		IP_H_V_Num	<= 0;
	end if;
end if;
if IP_H_View then
	LEDR(17)<='0';
	SEGS1	<= IP_History(IP_H_V_Num);
	SEGS2	<= std_logic_vector(to_unsigned(IP_H_V_Num,8));
	case sw(17 downto 15) is
		when "000"	=>	SEGS3	<= raw_ctrl_xfers(to_int(sw(9 downto 7))).bmRequestType;
		when "001"	=>	SEGS3	<= raw_ctrl_xfers(to_int(sw(9 downto 7))).bRequest;
		when "010"	=>	SEGS3	<= raw_ctrl_xfers(to_int(sw(9 downto 7))).wValue(7 downto 0);
		when "011"	=>	SEGS3	<= raw_ctrl_xfers(to_int(sw(9 downto 7))).wValue(15 downto 8);
		when "100"	=>	SEGS3	<= raw_ctrl_xfers(to_int(sw(9 downto 7))).wIndex(7 downto 0);
		when "101"	=>	SEGS3	<= raw_ctrl_xfers(to_int(sw(9 downto 7))).wIndex(15 downto 8);
		when "110"	=>	SEGS3	<= raw_ctrl_xfers(to_int(sw(9 downto 7))).wLength(7 downto 0);
		when "111"	=>	SEGS3	<= raw_ctrl_xfers(to_int(sw(9 downto 7))).wLength(15 downto 8);
	end case;
	if keylast(2)='0' and key(2) = '1' then
		IP_H_V_Num	<= IP_H_V_Num+1;
	elsif keylast(1)='0' and key(1) = '1' then
		IP_H_V_Num	<= IP_H_V_Num-1;
	end if;
else
	LEDR(17)<='1';
	if sw(1 downto 0) = "00" then
		SEGS1	<= w_data32(to_integer(unsigned(sw(14 downto 10))));--OTG_DcInterrupt(31 downto 16);
	elsif sw(1 downto 0) = "01" then
		SEGS1	<= sent_data(to_integer(unsigned(sw(14 downto 10))));
	elsif sw(1 downto 0) = "10" then
		SEGS1	<= EP1_Buff(to_integer(unsigned(sw(14 downto 10))));--
	end if;

	SEGS2	<= IP(7 downto 0);
	SEGS3	<= OTG_ESR(0);
	if IP/=IP_History(0) then
		IP_History(31)<=IP_History(30);IP_History(30)<=IP_History(29);IP_History(29)<=IP_History(28);IP_History(28)<=IP_History(27);IP_History(27)<=IP_History(26);IP_History(26)<=IP_History(25);IP_History(25)<=IP_History(24);IP_History(24)<=IP_History(23);IP_History(23)<=IP_History(22);IP_History(22)<=IP_History(21);IP_History(21)<=IP_History(20);IP_History(20)<=IP_History(19);IP_History(19)<=IP_History(18);IP_History(18)<=IP_History(17);IP_History(17)<=IP_History(16);IP_History(16)<=IP_History(15);IP_History(15)<=IP_History(14);IP_History(14)<=IP_History(13);IP_History(13)<=IP_History(12);IP_History(12)<=IP_History(11);IP_History(11)<=IP_History(10);IP_History(10)<=IP_History(9);IP_History(9) <=IP_History(8);IP_History(8) <=IP_History(7);IP_History(7) <=IP_History(6);IP_History(6) <=IP_History(5);IP_History(5) <=IP_History(4);IP_History(4) <=IP_History(3);IP_History(3) <=IP_History(2);IP_History(2) <=IP_History(1);IP_History(1) <=IP_History(0);IP_History(0) <=IP;
	end if;
end if;
LEDR(15 downto 0)	<= OTG_DcInterrupt(15 downto 0);

if SP = "00001" then
	LEDR(16) <= '1';
	IP_H_View	<= true;
end if;
if e_state = default then
	ledg(2 downto 0) <= "100";
elsif e_state = address then
	ledg(2 downto 0) <= "110";
else
	ledg(2 downto 0) <= "111";
end if;
LEDG(8)<=OTG_INT1;

		if key(0) = '0' then
			jump(x"0000");
LEDR(16)<='0';
		else
		case ip is
--=-=-=-=-=-MAIN LOOP STARTS HERE-=-=-=-=-=--
			when x"0000"	=>		--reset cpu
				reset_cpu;
			when x"0001"	=>
				go_sub(sub_reset_Dc);
			when x"0002"	=>
				go_sub(sub_init_isp1362);
			when x"0003"	=>
				wait_here(x"09C4");
			when x"0004"	=>--0111111100000101
				if OTG_INT1_latch = '1' then
					go_sub(sub_DcInterrupt);
				else
					jump(IP);	--wait here;
				end if;
			when x"0005"	=>--interrupt in ax
				if OTG_DcInterrupt(0) = '1' then	--reset
					jump(x"0000");--0003");
				elsif OTG_DcInterrupt(2) = '1' or OTG_DcInterrupt(7) = '1' then --suspend detected
					go_sub(sub_suspender);
				elsif OTG_DcInterrupt(8) = '1' then --ctrlOut is paging
					go_sub(sub_ctrlOut_handler);
				elsif OTG_DcInterrupt(14 downto 10)/="00000" then	--endpoint paging
					go_sub(sub_ep_int_handler);
				else
					jump(IP);	--i.e. Lock Up here
				end if;
			when x"0006"	=>
				jump(x"0004");
--=-=-=-=-=-PROGRAMMATIC SUBROUTINES-=-=-=-=-=--
--GO_SUB(x"1000");reset_dc--resets isp1362
			when x"1000"	=>
				OTG_RST_N	<= '0';
			when x"1001"	=>
				wait_here(x"09C4");	--clock at 25MHz => cycle lasts 40ns.  we need to wait 100us. 100/.04=2500
			when x"1002"	=>
				OTG_RST_N	<= '1';
			when x"1003"	=>
				ret_sub;
--GO_SUB(x"1100");port_out_cmd--
			when x"1100"	=>	--0ns
				OTG_ADDR(0)	<= '1';
				OTG_CS_N		<= '0';
				OTG_RD_N		<= '1';
				OTG_WR_N		<= '0';
				OTG_DATA		<= AX;
			when x"1101"	=>	--40ns
				OTG_WR_N	<= '1';
			when x"1102"	=>	--80ns
				OTG_CS_N	<= '1';
			when x"1103"	=>	--120ns
			when x"1104"	=>	--160ns
				OTG_DATA		<= "ZZZZZZZZZZZZZZZZ";
				ret_sub;
--GO_SUB(x"1200");port_out--
			when x"1200"	=>	--0ns
				OTG_ADDR(0)	<= '0';
				OTG_CS_N		<= '0';
				OTG_RD_N		<= '1';
				OTG_WR_N		<= '0';
				OTG_DATA		<= AX;
			when x"1201"	=>	--40ns
				OTG_WR_N	<= '1';
			when x"1202"	=>	--80ns
				OTG_CS_N	<= '1';
			when x"1203"	=>	--120ns
				OTG_DATA		<= "ZZZZZZZZZZZZZZZZ";
				ret_sub;
--GO_SUB(x"1300");port_in--
			when x"1300"	=>	--0ns
				OTG_ADDR(0)	<= '0';
				OTG_CS_N		<= '0';
				OTG_RD_N		<= '0';
				OTG_WR_N		<= '1';
			when x"1301"	=>	--40ns
				nAX	<= OTG_DATA;
			when x"1302"	=>	--80ns
				OTG_RD_N	<= '1';
				OTG_CS_N	<= '1';
			when x"1303"	=>	--120ns
				OTG_DATA		<= "ZZZZZZZZZZZZZZZZ";
				ret_sub;
--GO_SUB(x"1400");send_data_to_endpoint_in_DL(3 downto 0)--
			when x"1400"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_Buffer & DL(3 downto 0);
			when x"1401"	=>
				go_sub(sub_port_out);
				nAX	<= w_length;
			when x"1402"	=>
				go_sub(sub_port_dump);
			when x"1403"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Validate & DL(3 downto 0);
			when x"1404"	=>
				ret_sub;
--GO_SUB(x"1500");port_dump_from_data32	number of bytes in w_Length
			when x"1500"	=>
				w_count	<= 0;
				nBX	<= x"0000";
				if w_Length = x"0000" then
					ret_sub;
				end if;
			when x"1501"	=>	--0ns
				OTG_ADDR(0)		<= '0';
				OTG_CS_N		<= '0';
				OTG_RD_N		<= '1';
				OTG_WR_N		<= '0';
				OTG_DATA		<= w_data32(w_count);
			when x"1502"	=>	--40ns
				OTG_WR_N		<= '1';
sent_data(w_count)<=OTG_DATA;
			when x"1503"	=>	--80ns
				OTG_CS_N		<= '1';
				w_count<=w_count+1;
			when x"1504"	=>	--120ns
			when x"1505"	=>	--160ns
if w_count < (to_int(w_length)+1)/2 then
	jump(x"1501");
end if;
			when x"1506"	=>
				w_count	<= 0;
				OTG_DATA<= "ZZZZZZZZZZZZZZZZ";
				ret_sub;
--GO_SUB(x"1600");read_cfg_regs
			when x"1600"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Rd_DcInterruptEnable;
			when x"1601"	=>
				go_sub(sub_port_in);
			when x"1602"	=>
				OTG_DcInterruptEnable(15 downto 0)	<= AX;
				go_sub(sub_port_in);
			when x"1603"	=>
				OTG_DcInterruptEnable(31 downto 16)	<= AX;
				go_sub(sub_port_out_cmd);
				nAX	<= Rd_DcHardwareConfiguration;
			when x"1604"	=>
				go_sub(sub_port_in);
			when x"1605"	=>
				OTG_DcHardwareConfiguration	<= AX;
				go_sub(sub_port_out_cmd);
				nAX	<= Rd_DcMode;
			when x"1606"	=>
				go_sub(sub_port_in);
			when x"1607"	=>
				OTG_DcMode	<= AL;
				ret_sub;
--GO_SUB(x"1700");CRwrite	--writes value from Reg_DcRegister commanded by AX into register
			when x"1700"	=>
				go_sub(sub_port_out_cmd);
				nDX	<= AX;
			when x"1701"	=>
				go_sub(sub_port_out);
				case AX is
					when Wr_DcAddress				=>	--byte
						nAL	<= OTG_DcAddress;
					when Wr_DcMode					=>	--byte
						nAL	<= OTG_DcMode;
					when Wr_DcHardwareConfiguration	=>	--word
						nAX	<= OTG_DcHardwareConfiguration;
					when Wr_DcInterruptEnable		=>	--dword
						nAX	<= OTG_DcInterruptEnable(15 downto 0);
					when UnlockDevice	=>	--byte (special)
						nAX	<= x"AA37";
					when others	=>
						if AX(15 downto 4)=Wr_DcEndpointConfiguration then
							nAX	<= OTG_DcEndpointConfiguration(to_integer(unsigned(AX(3 downto 0))));
						end if;
				end case;
			when x"1702"	=>
				if DX = Wr_DcInterruptEnable then
					nAX	<= OTG_DcInterruptEnable(31 downto 16);
					go_sub(sub_port_out);
				else
					ret_sub;
				end if;
			when x"1703"	=>
				ret_sub;
--GO_SUB(x"1800");rcv_setup	--recieves 8 bytes from ctrlOut to w_ctrl_xfer
			when x"1800"	=>
				nAX	<= Rd_Buffer & ctrlOut;
				go_sub(sub_port_out_cmd);
			when x"1801"	=>
				go_sub(sub_port_in);
			when x"1802"	=>
				go_sub(sub_port_in);
raw_ctrl_xfers(7)<=raw_ctrl_xfers(6);
raw_ctrl_xfers(6)<=raw_ctrl_xfers(5);
raw_ctrl_xfers(5)<=raw_ctrl_xfers(4);
raw_ctrl_xfers(4)<=raw_ctrl_xfers(3);
raw_ctrl_xfers(3)<=raw_ctrl_xfers(2);
raw_ctrl_xfers(2)<=raw_ctrl_xfers(1);
raw_ctrl_xfers(1)<=raw_ctrl_xfers(0);
			when x"1803"	=>
				w_ctrl_xfer.bmRequestType	<= AL;
				w_ctrl_xfer.bRequest		<= AH;
				go_sub(sub_port_in);
			when x"1804"	=>
				w_ctrl_xfer.wValue			<= AX;
				go_sub(sub_port_in);
			when x"1805"	=>
				w_ctrl_xfer.wIndex			<= AX;
				go_sub(sub_port_in);
			when x"1806"	=>
				w_ctrl_xfer.wLength			<= AX;
				nAX	<= AcknowledgeSetup;
				go_sub(sub_port_out_cmd);
			when x"1807"	=>
raw_ctrl_xfers(0)<=w_ctrl_xfer;
				nAX	<= ClearBuffer & ctrlOut;
				go_sub(sub_port_out_cmd);
			when x"1808"	=>
				ret_sub;
--GO_SUB(x"1900");init_isp1362	--initilizes isp1362 DcRegisters
			when x"1900"	=>
				go_sub(sub_rd_cfg_regs);
			when x"1901"	=>
				OTG_DcMode					<= (OTG_DcMode and x"D2") or x"09";
				OTG_DcHardwareConfiguration	<= (OTG_DcHardwareConfiguration and x"8014") or x"20E1";
				OTG_DcInterruptEnable		<= (OTG_DcInterruptEnable and x"11000080") or x"00007D06";
				go_sub(sub_CRwrite);
				nAX	<= Wr_DcMode;
			when x"1902"	=>
				go_sub(sub_CRwrite);
				nAX	<= Wr_DcHardwareConfiguration;
			when x"1903"	=>
				go_sub(sub_CRwrite);
				nAX	<= Wr_DcInterruptEnable;
			when x"1904"	=>
				ret_sub;
--GO_SUB(x"1A00");Disp_Cfg_Regs	--displays masked Dcmode, DcHardwareConfiguration and last word of DcInterruptEnable
			when x"1A00"	=>
				go_sub(sub_rd_cfg_regs);
			when x"1A01"	=>
--				segs1	<= (OTG_DcInterruptEnable(15 downto 0) and x"FF7F");
				segs2	<= (OTG_DcHardwareConfiguration(7 downto 0) and x"EB");
				segs3	<= (OTG_DcHardwareConfiguration(15 downto 8) and x"7F");
				LEDR(15 downto 0)<= x"00" & (OTG_DcMode and x"2D");
				ret_sub;
--GO_SUB(x"");DcInterrupt	--loads OTG_DcInterrupt from isp1362
			when x"1B00"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Rd_DcInterrupt;
			when x"1B01"	=>
				go_sub(sub_port_in);
			when x"1B02"	=>
				OTG_DcInterrupt(15 downto 0)	<= AX;
				go_sub(sub_port_in);
			when x"1B03"	=>
				OTG_DcInterrupt(31 downto 16)	<= AX;
				ret_sub;
--GO_SUB(x"");sub_suspender
			when x"1C00"	=>
				go_sub(sub_DcInterrupt);
			when x"1C01"	=>
				if (OTG_DcInterrupt(2)='1' and OTG_DcInterrupt(7)='1') then
					go_sub(sub_rd_cfg_regs);
				else
					ret_sub;
				end if;
			when x"1C02"	=>
				go_sub(sub_CRwrite);
				nAX	<= Wr_DcMode or "00100000";
			when x"1C03"	=>
				go_sub(sub_CRwrite);
				nAX	<= Wr_DcMode and "11011111";
			when x"1C04"	=>
				wait_here(x"FFFF");	--5 ms before bus will wake up for sure
			when x"1C05"	=>
				wait_here(x"E847");	--(rest of 5ms)
			when x"1C06"	=>
				if OTG_INT1_latch = '1' then
					wait_here(x"09C4");	--100us to wake up
				else
					jump(IP);--MODIFIED MODIFIED MODIFIED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ADDED THIS
				end if;
			when x"1C07"	=>
				go_sub(sub_CRwrite);
				nAX	<= UnlockDevice;
			when x"1C08"	=>
				go_sub(sub_DcInterrupt);
			when x"1C09"	=>
				if OTG_DcInterrupt(7)='1' then
					jump(x"1C06");
				else
					ret_sub;
				end if;
--GO_SUB(x"1D00");ctrlOut_handler	--handles an interrupt by ctrlOut
			when x"1D00"	=>
				go_sub(sub_get_esr);
				nAX	<= Rd_ESR & ctrlOut;
			when x"1D01"	=>
				if AL(2)='1' and AL(3)='0' and AL(5)='1' then --setup packet ready
					go_sub(sub_rcv_setup);
				else	--error
					ret_sub;
				end if;
			when x"1D02"	=>
				case w_ctrl_xfer.bRequest is
					when GET_DESCRIPTOR	=>
						case w_ctrl_xfer.wValue(15 downto 8) is
							when desc_DEVICE	=>
								if (w_ctrl_xfer.bmRequestType=x"80"	and w_ctrl_xfer.wIndex=x"0000" and w_ctrl_xfer.wValue(7 downto 0)=x"00" ) then
									port_dump(ctrlIn,byte_deviceDescriptor(CRD_devDesc),w_ctrl_xfer.wLength);
								else
									go_sub(sub_port_out_cmd);
									nAX	<= Stall & ctrlIn;
								end if;
							when desc_STRING	=>
	ledg(7)<='1';
								case w_ctrl_xfer.wValue(7 downto 0) is
									when x"00"	=>
										port_dump(ctrlIn,CRD_strDesc_00_Langs,w_ctrl_xfer.wLength);
									when x"01"	=>
										if w_ctrl_xfer.wIndex = x"0409" then
											port_dump(ctrlIn,CRD_strDesc_01_Vendor);
										else
											go_sub(sub_port_out_cmd);
											nAX	<= Stall & ctrlIn;
										end if;
									when x"02"	=>
										if w_ctrl_xfer.wIndex = x"0409" then
											port_dump(ctrlIn,CRD_strDesc_02_Product,w_ctrl_xfer.wLength);
										else
											go_sub(sub_port_out_cmd);
											nAX	<= Stall & ctrlIn;
										end if;
									when x"03"	=>
										if w_ctrl_xfer.wIndex = x"0409" then
											port_dump(ctrlIn,CRD_strDesc_03_Serial,w_ctrl_xfer.wLength);
										else
											go_sub(sub_port_out_cmd);
											nAX	<= Stall & ctrlIn;
										end if;
									when others	=>	--CRD_strDesc_03_Serial
										go_sub(sub_port_out_cmd);
										nAX	<= Stall & ctrlIn;
								end case;
							when desc_CONFIGURATION	=>
								if w_ctrl_xfer.wValue(7 downto 0) = x"00" then
--									port_dump(ctrlIn,CRD_Full_Cfg_Desc,w_ctrl_xfer.wLength);
									port_dump(ctrlIn,CRD_Full_Cfg1_Desc,w_ctrl_xfer.wLength);
								elsif w_ctrl_xfer.wValue(7 downto 0) = x"01" then
									port_dump(ctrlIn,CRD_Full_Cfg2_Desc,w_ctrl_xfer.wLength);
								else
									go_sub(sub_port_out_cmd);
									nAX	<= Stall & ctrlIn;
								end if;
							when others	=>
								go_sub(sub_port_out_cmd);
								nAX	<= Stall & ctrlIn;
						end case;
					when SET_ADDRESS	=>
						if e_state = configured then
							ret_sub;
						else
							go_sub(sub_SET_ADDRESS);
						end if;
					when SET_CONFIGURATION	=>
						if e_state = default then
							ret_sub;
						elsif w_ctrl_xfer.wValue = x"0000" then
							e_state <= address;
							port_dump(ctrlIn,x"0000",x"0000");
						elsif w_ctrl_xfer.wValue = x"0001" or w_ctrl_xfer.wValue = x"0002" then
							what_config<=w_ctrl_xfer.wValue(7 downto 0);
							go_sub(sub_configureEps_n_ack);
						else
							go_sub(sub_port_out_cmd);
							nAX	<= Stall & ctrlIn;
						end if;
					when GET_STATUS	=>
						if e_state = default then
							ret_sub;
						elsif w_ctrl_xfer.bmRequestType = x"80" then
							port_dump(ctrlIn, x"0001");
						elsif w_ctrl_xfer.bmRequestType = x"81" and w_ctrl_xfer.wIndex = x"0000" then
							port_dump(ctrlIn, x"0000");
						elsif ((e_state=address and w_ctrl_xfer.wIndex = x"0000") or (e_state = configured)) and (w_ctrl_xfer.bmRequestType=x"82") then
							go_sub(sub_sendEpStatus);
						else
							go_sub(sub_port_out_cmd);
							nAX	<= Stall & ctrlIn;
						end if;
					when CLEAR_FEATURE	=>
						if e_state = default then
							ret_sub;
						elsif (w_ctrl_xfer.bmRequestType = x"02") and (w_ctrl_xfer.wValue=x"0000") then
							case(w_ctrl_xfer.wIndex(7 downto 0)) is
								when x"80"|x"00"=>	--ctrlInOut
									nAX	<=(Unstall & ctrlIn);
								when x"81"		=>	--ep1
									nAX	<=(Unstall & ep1);
								when x"02"		=>	--ep2
									nAX	<=(Unstall & ep2);
								when x"83"		=>	--ep3
									nAX	<=(Unstall & ep3);
								when x"04"		=>	--ep4
									nAX	<=(Unstall & ep4);
								when x"85"		=>	--ep5
									nAX	<=(Unstall & ep5);
								when others		=>	--unsupported or error
									nAX	<=(Stall & ctrlIn);
							end case;
							go_sub(sub_port_out_cmd);
						else
							nAX	<=(Stall & ctrlIn);
							go_sub(sub_port_out_cmd);
						end if;
--NEED TO ACK AFTER SET AND CLEAR FEATURES!!!!!!!!!!1
					when SET_FEATURE	=>
						if e_state = default then
							ret_sub;
						elsif (w_ctrl_xfer.bmRequestType = x"02") and (w_ctrl_xfer.wValue=x"0000") then
							case(w_ctrl_xfer.wIndex(7 downto 0)) is
								when x"81"		=>	--ep1
									nAX	<=(Stall & ep1);
								when x"02"		=>	--ep2
									nAX	<=(Stall & ep2);
								when x"83"		=>	--ep3
									nAX	<=(Stall & ep3);
								when x"04"		=>	--ep4
									nAX	<=(Stall & ep4);
								when x"85"		=>	--ep5
									nAX	<=(Stall & ep5);
								when others		=>	--unsupported or error
									nAX	<=(Stall & ctrlIn);
							end case;
							go_sub(sub_port_out_cmd);
						else
							nAX	<=(Stall & ctrlIn);
							go_sub(sub_port_out_cmd);
						end if;
					when GET_CONFIGURATION	=>
						if e_state = default then
							ret_sub;
						elsif e_state = address then
							port_dump(ctrlIn,x"00");
						elsif e_state = configured then
							port_dump(ctrlIn,what_config);
						end if;
					when GET_INTERFACE|SET_INTERFACE	=>
						if (e_state=address)or((w_ctrl_xfer.wIndex(7 downto 0)/=x"00")) then
							nAX<=(Stall & ctrlIn);
							go_sub(sub_port_out_cmd);
						else
							if w_ctrl_xfer.bRequest = GET_INTERFACE then
								w_length<=x"0001";
								loadBuffer(x"00"&which_interface);
							elsif w_ctrl_xfer.wValue(7 downto 1) = "0000000" then
								which_interface <= w_ctrl_xfer.wValue(7 downto 0);
								w_length<= x"0000";
								go_sub(sub_send_data);
							else
								nAX<=(Stall & ctrlIn);
								go_sub(sub_port_out_cmd);
							end if;
						end if;
					when others	=>
jump(x"FFFF");--LOCK UP and display unrecognized request
				end case;
			when x"1D03"	=>
				ret_sub;
--GO_SUB(x"");get_ESR	--reads an ESR specified by command in AL into its corresponding OTG_ESR register and AL
			when x"1E00"	=>	--7:stall 6:2ndary full 5:primary full 4:PID 3:missed_setup go home 2:setup_pkt 1:2ndary selected 0:none
				go_sub(sub_port_out_cmd);
				nBX	<= AX;
			when x"1E01"	=>
				go_sub(sub_port_in);
			when x"1E02"	=>
				OTG_ESR(to_integer(unsigned(BL(3 downto 0))))<= AL;
				ret_sub;
--GO_SUB(x"1F00");sub_SET_ADDRESS
			when x"1F00"	=>
				if e_state = configured then
					ret_sub;
				else
					go_sub(sub_port_out_cmd);
					nAX	<= Wr_DcAddress;
				end if;
			when x"1F01"	=>
				go_sub(sub_port_out);
				nAX	<= x"0080" or w_ctrl_xfer.wValue;
				if w_ctrl_xfer.wValue = x"0000" then
					e_state <= default;
				else
					e_state <= address;
				end if;
			when x"1F02"	=>
				nAX	<= Wr_Buffer & ctrlIn;
				go_sub(sub_port_out_cmd);
			when x"1F03"	=>
				nAX	<= x"0000";
				go_sub(sub_port_out);
			when x"1F04"	=>
				nAX	<= Validate & ctrlIn;
				go_sub(sub_port_out_cmd);
			when x"1F05"	=>
				ret_sub;
--GO_SUB(x"2000");sub_configureEps_n_ack
			when x"2000"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ctrlOut;
			when x"2001"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(0);
			when x"2002"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ctrlIn;
			when x"2003"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(1);
			when x"2004"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ep1;
			when x"2005"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(2);
			when x"2006"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ep2;
			when x"2007"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(3);
			when x"2008"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ep3;
			when x"2009"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(4);
			when x"200A"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ep4;
			when x"200B"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(5);
			when x"200C"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ep5;
			when x"200D"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(6);
			when x"200E"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ep6;
			when x"200F"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(7);
			when x"2010"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ep7;
			when x"2011"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(8);
			when x"2012"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ep8;
			when x"2013"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(9);
			when x"2014"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ep9;
			when x"2015"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(10);
			when x"2016"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ep10;
			when x"2017"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(11);
			when x"2018"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ep11;
			when x"2019"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(12);
			when x"201A"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ep12;
			when x"201B"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(13);
			when x"201C"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ep13;
			when x"201D"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(14);
			when x"201E"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= Wr_DcEndpointConfiguration & ep14;
			when x"201F"	=>
				go_sub(sub_port_out);
				nAL	<= DcEndpointConfiguration(15);
			when x"2020"	=>
				e_state <= configured;
				port_dump(ctrlIn,x"0000",x"0000");
			when x"2021"	=>
				ret_sub;
--GO_SUB(x"2200");sub_sendEpStatus
			when x"2200"	=>
				case w_ctrl_xfer.windex(7 downto 0) is
					when x"80"|x"00"=>	--ctrlInOut
						go_sub(sub_port_out_cmd);
						nAX	<= (Rd_DcEndpointStatusImage & ctrlIn);
					when x"81"		=>	--ep1
						go_sub(sub_port_out_cmd);
						nAX	<= (Rd_DcEndpointStatusImage & ep1);
					when x"02"		=>	--ep2
						go_sub(sub_port_out_cmd);
						nAX	<= (Rd_DcEndpointStatusImage & ep2);
					when x"83"		=>	--ep3
						go_sub(sub_port_out_cmd);
						nAX	<= (Rd_DcEndpointStatusImage & ep3);
					when x"04"		=>	--ep4
						go_sub(sub_port_out_cmd);
						nAX	<= (Rd_DcEndpointStatusImage & ep4);
					when x"85"		=>	--ep5
						go_sub(sub_port_out_cmd);
						nAX	<= (Rd_DcEndpointStatusImage & ep5);
					when others		=>	--unsupported or error
						jump(x"2210");
				end case;
			when x"2201"	=>
				go_sub(sub_send_data);
				w_length<=x"0002";
				loadBuffer(x"000"&"000"&AL(7));
			when x"2202"	=>
				ret_sub;
			when x"2203"	=>
			when x"2204"	=>
			when x"2205"	=>
			when x"2206"	=>
			when x"2207"	=>
			when x"2208"	=>
			when x"2209"	=>
			when x"220A"	=>
			when x"220B"	=>
			when x"220C"	=>
			when x"220D"	=>
			when x"220E"	=>
			when x"220F"	=>
			when x"2210"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= (Stall & ctrlIn);
			when x"2211"	=>
				ret_sub;

--GO_SUB(x"2300");sub_EP_Int_Handler  handles endpoing interrupts OTG_DcInterrupt(14 downto 10)/="00000"
			when x"2300"	=>
				if OTG_DcInterrupt(10)='1' then		--EP1	--64b Bulk Out			signal EP1_Buffer	: buffer64
					go_sub(sub_get_esr);
					nAX	<= Rd_ESR & ep1;
				elsif OTG_DcInterrupt(11)='1' then	--EP2	--64b Bulk In			signal EP2_Buffer	: buffer64
					go_sub(sub_get_esr);
					nAX	<= Rd_ESR & ep2;
				elsif OTG_DcInterrupt(12)='1' then	--EP3	--16b Int Out			signal EP3_Buffer	: buffer16
					go_sub(sub_get_esr);
					nAX	<= Rd_ESR & ep3;
				elsif OTG_DcInterrupt(13)='1' then	--EP4	--16b Int In			signal EP4_Buffer	: buffer16
					go_sub(sub_get_esr);
					nAX	<= Rd_ESR & ep4;
				elsif OTG_DcInterrupt(14)='1' then	--EP5	--1023b Iso In (dblBuff) (IMPLEMENT SRAM BUFFER)
					go_sub(sub_get_esr);
					nAX	<= Rd_ESR & ep5;
				end if;
			when x"2301"	=>
				if OTG_DcInterrupt(10)='1' then		--EP1	--64b Bulk Out			signal EP1_Buffer	: buffer64
					if AX(5)='1' then
						go_sub(x"2310");
					else
						ret_sub;
					end if;
				elsif OTG_DcInterrupt(11)='1' then	--EP2	--64b Bulk In			signal EP2_Buffer	: buffer64
					if AX(5)='0' then
						port_dump(ep2,EP2_Buffer);
					else
						ret_sub;
					end if;
				elsif OTG_DcInterrupt(12)='1' then	--EP3	--16b Int Out			signal EP3_Buffer	: buffer16
					if AX(5)='1' then
						go_sub(x"2320");
					else
						ret_sub;
					end if;
				elsif OTG_DcInterrupt(13)='1' then	--EP4	--16b Int In			signal EP4_Buffer	: buffer16
					if AX(5)='0' then
						port_dump(ep4,EP4_Buffer);
					else
						ret_sub;
					end if;
				elsif OTG_DcInterrupt(14)='1' then	--EP5	--1023b Iso In (dblBuff) (IMPLEMENT SRAM BUFFER)
					if AX(5)='0' then
						port_dump(ep5,EP2_Buffer);
					else
						ret_sub;
					end if;
				end if;
			when x"2302"	=>
				ret_sub;
			when x"2303"	=>
			when x"2304"	=>
			
			
			when x"2310"	=>
				nAX	<= Rd_Buffer & ep1;
				go_sub(sub_port_out_cmd);
			when x"2311"	=>
				go_sub(sub_port_in);	--read length
			when x"2312"	=>
				nCX	<= to_vec(16,((to_int(AX)+1) / 2));--num words to read
				nBX	<= x"0000";
			when x"2313"	=>
				go_sub(sub_port_in);	--THIS COULD BE IMPLEMENTED WAAAAAAAY FASTER (like sub_send_data)
			when x"2314"	=>
				EP1_Buff(to_int(BX))<=AX;	--signal EP1_Buffer	: buffer64
				nBX<=to_vec(16,to_int(BX)+1);
				loopJump(x"2313");
			when x"2315"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= ClearBuffer & ep1;
			when x"2316"	=>
				ret_sub;
			when x"2320"	=>
				nAX	<= Rd_Buffer & ep3;
				go_sub(sub_port_out_cmd);
			when x"2321"	=>
				go_sub(sub_port_in);	--read length
			when x"2322"	=>
				nCX	<= to_vec(16,((to_int(AX)+1) / 2));--num words to read
				nBX	<= x"0000";
			when x"2323"	=>
				go_sub(sub_port_in);	--THIS COULD BE IMPLEMENTED WAAAAAAAY FASTER (like sub_send_data)
			when x"2324"	=>
				EP3_Buff(to_int(BX))<=AX;	--signal EP3_Buffer	: buffer16
				nBX<=to_vec(16,to_int(BX)+1);
				loopJump(x"2313");
			when x"2325"	=>
				go_sub(sub_port_out_cmd);
				nAX	<= ClearBuffer & ep3;
			when x"2326"	=>
				ret_sub;
			
			when x"2400"	=>
			when x"2401"	=>
			when x"2402"	=>
			when x"2403"	=>
			when x"2404"	=>
			when x"2405"	=>
			when x"2406"	=>
			when x"2407"	=>
			when x"2408"	=>
			when x"2409"	=>
			when x"240A"	=>
			when x"240B"	=>
			when x"240C"	=>
			when x"240D"	=>
			when x"240E"	=>
			when x"240F"	=>
			when x"2500"	=>
			when x"2501"	=>
			when x"2502"	=>
			when x"2503"	=>
			when x"2504"	=>
			when x"2505"	=>
			when x"2506"	=>
			when x"2507"	=>
			when x"2508"	=>
			when x"2509"	=>
			when x"250A"	=>
			when x"250B"	=>
			when x"250C"	=>
			when x"250D"	=>
			when x"250E"	=>
			when x"250F"	=>
			when x"2600"	=>
			when x"2601"	=>
			when x"2602"	=>
			when x"2603"	=>
			when x"2604"	=>
			when x"2605"	=>
			when x"2606"	=>
			when x"2607"	=>
			when x"2608"	=>
			when x"2609"	=>
			when x"260A"	=>
			when x"260B"	=>
			when x"260C"	=>
			when x"260D"	=>
			when x"260E"	=>
			when x"260F"	=>
			when x"2700"	=>
			when x"2701"	=>
			when x"2702"	=>
			when x"2703"	=>
			when x"2704"	=>
			when x"2705"	=>
			when x"2706"	=>
			when x"2707"	=>
			when x"2708"	=>
			when x"2709"	=>
			when x"270A"	=>
			when x"270B"	=>
			when x"270C"	=>
			when x"270D"	=>
			when x"270E"	=>
			when x"270F"	=>
			when x"FFFF"	=>
				jump(x"FFFF");
			when others	=>
				jump(IP);
		end case;
		end if;
end process worker;

end handler;