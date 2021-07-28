#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul 28 13:09:00 2021

@author: sjguo
"""

import 

def shadow(channel, shots, noise=False, seed=None, output="Prep-Meas", adaptive=False):
    '''
    Apply shadow tomography to a quantum channel defined as a qiskit circuit.
    
    (For qiskit, noise is defined in the backend simulator, which means initial
    state preparation and rotation before measurement will also suffer from it. 
    Need to think: This is more realistic, but may also over complicated.)
    
    Things look helpful:
        https://qiskit.org/documentation/stubs/qiskit.quantum_info.Choi.html#qiskit.quantum_info.Choi
        https://qiskit.org/documentation/tutorials/noise/8_tomography.html
        tomography package contains a most generalized function _tomography_circuits:
            https://qiskit.org/documentation/_modules/qiskit/ignis/verification/tomography/basis/circuits.html
    
    Parameters
    ----------
    channel : qiskit.QuantumCircuit
        Quantum channel defined by quantum circuit.
    shots : int
        Number of measurements.
    noise : qiskit.providers.aer.noise.NoiseModel, or False, optional
        Noise that channel suffered. The default is False.
    seed : int or None, optional
        Random seed. The default is None.
    output : str, optional
        "Prep-Meas": Output preparation Pauli string, bits, and measurement Pauli
        string and measured result. The default is "Prep-Meas".
        "Est-Channel": Output estimated channel operator.
        "Diamond-Norm": Output diamond norm between estimated channel operator
        and ground truth channel.
    adaptive : Boolean, optional
        Future feature: proceeds adaptive shadow tomography. The default is False.

    Returns
    -------
    res. Depends on "output".

    '''
    # Number of qubits
    n_qubit = 
    # Ramdom Pauli strings for Prep and Meas.
    paulis = [0, 1]
    # Ramdom Bits for Prep
    bits = [0]
    # Define empty measurement
    meas = []
    for s in range(shots):
        # Construct full circuit
        
        # Execute and measure
        m = 1
        meas.append(m)
    
    if output=="Prep-Meas": # return prep and meas
        return [paulis[0], bits, paulis[1], meas]
    else:
        return