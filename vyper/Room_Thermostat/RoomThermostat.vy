# @version ^0.3.5

enum StateType:
    CREATED
    IN_USE

enum ModeType:
    OFF
    COOL
    HEAT
    AUTO

INITIAL_TARGET_TEMP: constant(int128) = 70

state: public(StateType)

installer: public(address)
user: public(address)

target_temperature: public(int128) 
mode: public(ModeType)

@external
def __init__(thermostat_installer: address, thermostat_user: address):
    self.installer = thermostat_installer
    self.user = thermostat_user
    self.target_temperature = INITIAL_TARGET_TEMP
    self.state = StateType.CREATED

@external
def start_thermostat():
    assert self.installer == msg.sender and self.state == StateType.CREATED, "Was not created or it was not the installer"
    
    self.state = StateType.IN_USE

@external
def set_target_temperature(target_temp: int128):
    assert self.user == msg.sender and self.state == StateType.IN_USE, "Cannot set temperature, cause it is not the user or the thermostat is not in use."

    self.target_temperature = target_temp

@external
def set_mode(modeValue: ModeType):
    assert self.user == msg.sender and self.state == StateType.IN_USE, "Cannot set mode, cause it is not the user or the thermostat is not in use."

    self.mode = modeValue
    # maybe change temp according with the mode