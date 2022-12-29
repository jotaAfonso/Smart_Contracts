# @version ^0.3.5

event Workbench_Contract_Create:
    application_name: String[100]
    workflow_name: String[100]
    origin_address: address

event Workbench_Contract_Update:
    application_name: String[100]
    workflow_name: String[100]
    action: String[100]
    origin_address: address

application_name: String[100]
workflow_name: String[100]

@external
def __init__(application_name_param: String[100], workflow_name_param: String[100]):
    self.application_name = application_name_param
    self.workflow_name = workflow_name_param

@internal
def contract_Created():
    log Workbench_Contract_Create(self.application_name, self.workflow_name, msg.sender)

@internal
def contract_Updated(action_param: String[100]):
    log Workbench_Contract_Update(self.application_name, self.workflow_name, action_param, msg.sender)