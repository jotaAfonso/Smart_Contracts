# @version ^0.3.5

enum StateType:
    REQUEST
    RESPOND

state: public(StateType)

requestor: public(address)
responder: public(address)

message_request: public(String[100])
message_response: public(String[100])

@external
def __init__(message: String[100]):
    self.requestor = msg.sender
    self.message_request = message
    self.state = StateType.REQUEST

@external
def send_request(message: String[100]):
    # only the requestor can send a request
    assert msg.sender == self.requestor, "Only the requestor can send requests."
    # passes if the current state is not request
    assert StateType.RESPOND == self.state, "Needs to send a response first."
    
    self.message_request = message
    self.state = StateType.REQUEST

@external
def get_request() -> String[100]:
    # only the responder can view a message request
    assert empty(address) == self.responder, "There exists no responders yet, send a reponse to be the first."
    # only the responder can view a message request
    assert msg.sender == self.responder, "Only the responder can view the message request."
    # only gets the message if it is not empty
    assert empty(String[100]) != self.message_request, "Message request is empty."

    return self.message_request

@external
def send_response(message: String[100]):
    # the requestor is different than the responder
    assert msg.sender != self.requestor, "Responder and requestor need to be different."
    # passes if the current state is not respond
    assert StateType.REQUEST == self.state, "Needs to send a request first."
    
    if self.responder == empty(address):
        self.responder = msg.sender

    # only the responder can send a response
    assert msg.sender == self.responder, "Only the responder can send responses."
    
    self.message_response = message
    self.state = StateType.RESPOND

@external
def get_response() -> String[100]:
    # only the requestor can view a message response
    assert msg.sender == self.requestor, "Only the requestor can view the message response."
    # only gets the message if it is not empty
    assert empty(String[100]) != self.message_request, "Message response is empty."

    return self.message_response