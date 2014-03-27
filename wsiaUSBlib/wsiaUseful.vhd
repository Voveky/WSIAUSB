--wsiaUSBlib
--a library required by wsiaUSB DE2 ISP1362 USB firmware by Tony Slagle

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
--use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

package wsiaUseful is 
--=-=-=-=-=-TYPE DECLARATIONS-=-=-=-=-=--
type enum_state		is (default, address, configured);
subtype nibble		is std_logic_vector(3 downto 0);
subtype byte		is std_logic_vector(7 downto 0);
subtype word		is std_logic_vector(15 downto 0);
subtype	dword		is std_logic_vector(31 downto 0);
subtype buffer64	is std_logic_vector(0 to 8*64-1);
subtype buffer16	is std_logic_vector(0 to 8*16-1);
subtype descript9	is std_logic_vector(0 to 8*9-1);
subtype descript7	is std_logic_vector(0 to 8*7-1);
type byte8	is array(0 to 7) of byte;
type byte16	is array(0 to 15) of byte;
type byte32	is array(0 to 31) of byte;
type word8	is array(0 to 7) of word;
type word16	is array(0 to 15) of word;
type word32	is array(0 to 31) of word;
type setup_packet_type is 
  record
    bmRequestType	: byte;
    bRequest		: byte;
    wValue			: word;
    wIndex			: word;
    wLength			: word;
  end record;

--type worker_states is (settle, idle, send_data, CRread, CRwrite, rd_word, wr_word, wr_cmd, rcv_setup);
type worker_states is (send_data, CRread, CRwrite, rd_word, wr_word, wr_cmd, rcv_setup);

function dizzy_indian	( constant data: in std_logic_vector ) return std_logic_vector;
function to_int 		( a: std_logic_vector ) return integer;
function to_vec 		( size: integer; val: integer ) return std_logic_vector;
function bigger			( l,r:std_logic_vector ) return std_logic_vector;
function smaller		( l,r:std_logic_vector ) return std_logic_vector;

--=-=-=-=-=-CONSTANT DECLARATIONS-=-=-=-=-=--
constant h0, CtrlOut: std_logic_vector(3 downto 0)	:= "0000"; constant h1, CtrlIn: std_logic_vector(3 downto 0):= "0001";
constant h2, ep1	: std_logic_vector(3 downto 0)	:= "0010"; constant h3, ep2	: std_logic_vector(3 downto 0)	:= "0011";
constant h4, ep3	: std_logic_vector(3 downto 0)	:= "0100"; constant h5, ep4	: std_logic_vector(3 downto 0)	:= "0101";
constant h6, ep5	: std_logic_vector(3 downto 0)	:= "0110"; constant h7, ep6	: std_logic_vector(3 downto 0)	:= "0111";
constant h8, ep7	: std_logic_vector(3 downto 0)	:= "1000"; constant h9, ep8	: std_logic_vector(3 downto 0)	:= "1001";
constant hA, ep9	: std_logic_vector(3 downto 0)	:= "1010"; constant hB, ep10: std_logic_vector(3 downto 0)	:= "1011";
constant hC, ep11	: std_logic_vector(3 downto 0)	:= "1100"; constant hD, ep12: std_logic_vector(3 downto 0)	:= "1101";
constant hE, ep13	: std_logic_vector(3 downto 0)	:= "1110"; constant hF, ep14: std_logic_vector(3 downto 0)	:= "1111";
--=-=-=-=-=-STANDARD REQUESTS-=-=-=-=-=--
constant GET_STATUS			: std_logic_vector(7 downto 0)	:= h0 & h0;
constant CLEAR_FEATURE		: std_logic_vector(7 downto 0)	:= h0 & h1;
constant SET_FEATURE		: std_logic_vector(7 downto 0)	:= h0 & h3;
constant SET_ADDRESS		: std_logic_vector(7 downto 0)	:= h0 & h5;
constant GET_DESCRIPTOR		: std_logic_vector(7 downto 0)	:= h0 & h6;
constant SET_DESCRIPTOR		: std_logic_vector(7 downto 0)	:= h0 & h7;
constant GET_CONFIGURATION	: std_logic_vector(7 downto 0)	:= h0 & h8;
constant SET_CONFIGURATION	: std_logic_vector(7 downto 0)	:= h0 & h9;
constant GET_INTERFACE		: std_logic_vector(7 downto 0)	:= h0 & hA;
constant SET_INTERFACE		: std_logic_vector(7 downto 0)	:= h0 & hB;
constant SYNCH_FRAME_E		: std_logic_vector(7 downto 0)	:= h0 & hC;
constant CFG_DEVICE			: std_logic_vector(3 downto 0) := h0;
constant CFG_INTERFACE		: std_logic_vector(3 downto 0) := h1;
constant CFG_ENDPOINT		: std_logic_vector(3 downto 0) := h2;
--=-=-=-=-=-COMMANDS-=-=-=-=-=--
constant Wr_DcEndpointConfiguration	: std_logic_vector(15 downto 4)	:= h0 & h0 & h2;		--20h = control out
constant Rd_DcEndpointConfiguration	: std_logic_vector(15 downto 4)	:= h0 & h0 & h3;		--30h = control out
constant Wr_DcAddress				: std_logic_vector(15 downto 0)	:= h0 & h0 & hB & h6;	--B6h
constant Rd_DcAddress				: std_logic_vector(15 downto 0)	:= h0 & h0 & hB & h7;	--B7h
constant Wr_DcMode					: std_logic_vector(15 downto 0)	:= h0 & h0 & hB & h8;	--B8h
constant Rd_DcMode					: std_logic_vector(15 downto 0)	:= h0 & h0 & hB & h9;	--B9h
constant Wr_DcHardwareConfiguration	: std_logic_vector(15 downto 0)	:= h0 & h0 & hB & hA;	--BAh
constant Rd_DcHardwareConfiguration	: std_logic_vector(15 downto 0)	:= h0 & h0 & hB & hB;	--BBh
constant Wr_DcInterruptEnable		: std_logic_vector(15 downto 0)	:= h0 & h0 & hC & h2;	--C2h
constant Rd_DcInterruptEnable		: std_logic_vector(15 downto 0)	:= h0 & h0 & hC & h3;	--C3h
constant Wr_DcDMAConfiguration		: std_logic_vector(15 downto 0)	:= h0 & h0 & hF & h0;	--F0h
constant Rd_DcDMAConfiguration		: std_logic_vector(15 downto 0)	:= h0 & h0 & hF & h1;	--F1h
constant Wr_DcDMACounter			: std_logic_vector(15 downto 0)	:= h0 & h0 & hF & h2;	--F2h
constant Rd_DcDMACounter			: std_logic_vector(15 downto 0)	:= h0 & h0 & hF & h3;	--F3h
constant Reset						: std_logic_vector(15 downto 0)	:= h0 & h0 & hF & h6;	--F6h
constant Wr_Buffer					: std_logic_vector(15 downto 4)	:= h0 & h0 & h0;		--00h = control out (0 illegal)
constant Rd_Buffer					: std_logic_vector(15 downto 4)	:= h0 & h0 & h1;		--10h = control out (1 illegal)
constant Rd_ESR						: std_logic_vector(15 downto 4)	:= h0 & h0 & h5;		--50h = control out
constant Stall						: std_logic_vector(15 downto 4)	:= h0 & h0 & h4;		--40h = control out
constant Unstall					: std_logic_vector(15 downto 4)	:= h0 & h0 & h8;		--80h = control out
constant Validate					: std_logic_vector(15 downto 4)	:= h0 & h0 & h6;		--60h = control out (0 illegal)
constant ClearBuffer				: std_logic_vector(15 downto 4)	:= h0 & h0 & h7;		--70h = control out (1 illegal)
constant Rd_DcEndpointStatusImage	: std_logic_vector(15 downto 4)	:= h0 & h0 & hD;		--D0h = control out
constant AcknowledgeSetup			: std_logic_vector(15 downto 0)	:= h0 & h0 & hF & h4;	--F4h (must ack setups sec12.3.6)
constant Rd_ErrorCode				: std_logic_vector(15 downto 4)	:= h0 & h0 & hA;		--A0h = control out
constant UnlockDevice				: std_logic_vector(15 downto 0)	:= h0 & h0 & hB & h0;	--B0h
constant Wr_DcScratchRegister		: std_logic_vector(15 downto 0)	:= h0 & h0 & hB & h2;	--B2h
constant Rd_DcScratchRegister		: std_logic_vector(15 downto 0)	:= h0 & h0 & hB & h3;	--B3h
constant Rd_DcFrameNumber			: std_logic_vector(15 downto 0)	:= h0 & h0 & hB & h4;	--B4h
constant Rd_DcChipID				: std_logic_vector(15 downto 0)	:= h0 & h0 & hB & h5;	--B5h
constant Rd_DcInterrupt				: std_logic_vector(15 downto 0)	:= h0 & h0 & hC & h0;	--C0h

end wsiaUseful; 

package body wsiaUseful is 
function dizzy_indian( constant data: in std_logic_vector ) return std_logic_vector is
	--flips byte order s.t. B0 B1 B2 B3 B4 would yield B1 B0 B3 B2 B? B4 where b? is a filler byte
	--always returns an even number of bytes
	variable num	: integer;
	variable outval	: std_logic_vector(0 to data'LENGTH-1);
begin
		for i in 0 to (data'LENGTH)/16-1 loop
			outval(i*16 to (i*16)+15):= data(i*16+8 to i*16+15) & data(i*16 to i*16+7);
		end loop;
		return outval;
end function dizzy_indian;


function to_int	(a: std_logic_vector) return integer is 
    alias av: std_logic_vector (1 to a'length) is a; 
    variable val: integer := 0; 
    variable b: integer := 1; 
begin 
    for i in a'length downto 1 loop 
        if (av(i) = '1') then    -- if LSB is '1', 
            val := val + b;       -- add value for current bit position 
        end if; 
        b := b * 2;    -- Shift left 1 bit 
    end loop; 
    return val; 
end function to_int; 
function to_vec	(size: integer; val: integer) return std_logic_vector is 
    variable vec: std_logic_vector (1 to size); 
    variable a: integer; 
begin 
    a := val; 
    for i in size downto 1 loop 
        if ((a mod 2) = 1) then 
            vec(i) := '1'; 
        else  
            vec(i) := '0'; 
        end if; 
        a := a / 2; 
    end loop; 
    return vec; 
end function to_vec; 
function bigger( l,r:std_logic_vector ) return std_logic_vector is
	alias ll :std_logic_vector(l'length-1 downto 0) is l;
	alias rr :std_logic_vector(r'length-1 downto 0) is r;
begin
	if l'length > r'length then
		if to_int(ll(l'length-1 downto r'length))/=0 then
			return(ll);
		else
			if to_int(ll(r'length-1 downto 0)) < to_int(rr) then
				return(ll(l'length-1 downto r'length) & rr);
			else
				return(ll);
			end if;
		end if;
	elsif r'LENGTH > l'LENGTH then
		if to_int(rr(r'length-1 downto l'length))/=0 then
			return(rr);
		else
			if to_int(rr(l'length-1 downto 0)) < to_int(ll) then
				return(rr(r'length-1 downto l'length) & ll);
			else
				return(rr);
			end if;
		end if;
	else
		if ll > rr then
			return(ll);
		else
			return(rr);
		end if;
	end if;
end function bigger;	
function smaller( l,r:std_logic_vector ) return std_logic_vector is
	alias ll :std_logic_vector(l'length-1 downto 0) is l;
	alias rr :std_logic_vector(r'length-1 downto 0) is r;
begin
	if l'length > r'length then
		if to_int(ll) > to_int(rr) then
			return(to_vec(l'length,to_int(rr)));
		else
			return(ll);
		end if;
	elsif r'length > l'length then
		if to_int(rr) > to_int(ll) then
			return(to_vec(r'length,to_int(ll)));
		else
			return(rr);
		end if;
	else
		if ll < rr then
			return(ll);
		else
			return(rr);
		end if;
	end if;
end function smaller;	
end wsiaUseful; 
