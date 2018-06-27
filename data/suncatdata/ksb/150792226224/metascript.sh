#!/bin/bash
# Slurm parameters
NTASKS=`echo $SLURM_TASKS_PER_NODE|tr '(' ' '|awk '{print $1}'`
NNODES=`scontrol show hostnames $SLURM_JOB_NODELIST|wc -l`
NCPU=`echo " $NTASKS * $NNODES " | bc`
# load gpaw-specific paths
source /scratch/users/ksb/gpaw/paths.bash
# run parallel gpaw
mpirun -n $NCPU gpaw-python script.py