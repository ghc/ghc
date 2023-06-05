#!/usr/bin/env python
# -*- coding: utf-8 -*-

from typing import List, Union, Dict
from collections import namedtuple

class FieldType:
    def __init__(self, c_type: str):
        self.c_type = c_type

VariableLength = None
ThreadId = FieldType('EventThreadID')
KernelThreadId = FieldType('EventKernelThreadId')
CapNo = FieldType('EventCapNo')
CapsetType = FieldType('EventCapsetType')
Timestamp = FieldType('EventTimestamp')
CapsetId = FieldType('EventCapsetID')
TaskId = FieldType('EventTaskId')

Word8  = FieldType('StgWord8')
Word16 = FieldType('StgWord16')
Word32 = FieldType('StgWord32')
Word64 = FieldType('StgWord64')

class EventType:
    def __init__(self,
                 n: int,
                 name: str,
                 fields: Union[VariableLength, List[FieldType]],
                 description: str):
        self.n = n
        self.name = name
        self.fields = fields
        self.description = description

    def __str__(self):
        return '(n={n}, name={name})'.format(n=self.n, name=self.name)

# When adding a new event type used by GHC you should also update
# NUM_GHC_EVENT_TAGS in rts/include/rts/EventLogFormat.h.
event_types = [
    EventType( 0, 'CREATE_THREAD',   [ThreadId],                      'Create thread'),
    EventType( 1, 'RUN_THREAD',      [ThreadId],                      'Run thread'),
    EventType( 2, 'STOP_THREAD',     [ThreadId, Word16, ThreadId],    'Stop thread'),
    EventType( 3, 'THREAD_RUNNABLE', [ThreadId],                      'Thread runnable'),
    EventType( 4, 'MIGRATE_THREAD',  [ThreadId, CapNo],               'Migrate thread'),
    # 5, 6, 7 deprecated
    EventType( 8, 'THREAD_WAKEUP',   [ThreadId, CapNo],               'Wakeup thread'),
    EventType( 9, 'GC_START',        [],                              'Starting GC'),
    EventType(10, 'GC_END',          [],                              'Finished GC'),
    EventType(11, 'REQUEST_SEQ_GC',  [],                              'Request sequential GC'),
    EventType(12, 'REQUEST_PAR_GC',  [],                              'Request parallel GC'),
    # 13, 14 deprecated
    EventType(15, 'CREATE_SPARK_THREAD', [ThreadId],                  'Create spark thread'),
    EventType(16, 'LOG_MSG',          VariableLength,                 'Log message'),
    # 17 deprecated
    EventType(18, 'BLOCK_MARKER',     [Word32, Timestamp, CapNo],     'Block marker'),
    EventType(19, 'USER_MSG',         VariableLength,                 'User message'),
    EventType(20, 'GC_IDLE',          [],                             'GC idle'),
    EventType(21, 'GC_WORK',          [],                             'GC working'),
    EventType(22, 'GC_DONE',          [],                             'GC done'),
    # 23, 24 used by eden
    EventType(25, 'CAPSET_CREATE',    [CapsetId, CapsetType],         'Create capability set'),
    EventType(26, 'CAPSET_DELETE',    [CapsetId],                     'Delete capability set'),
    EventType(27, 'CAPSET_ASSIGN_CAP',[CapsetId, CapNo],              'Add capability to capability set'),
    EventType(28, 'CAPSET_REMOVE_CAP',[CapsetId, CapNo],              'Remove capability from capability set'),
    EventType(29, 'RTS_IDENTIFIER',   VariableLength,                 'RTS name and version'),
    EventType(30, 'PROGRAM_ARGS',     VariableLength,                 'Program arguments'),
    EventType(31, 'PROGRAM_ENV',      VariableLength,                 'Program environment variables'),
    EventType(32, 'OSPROCESS_PID',    [CapsetId, Word32],             'Process ID'),
    EventType(33, 'OSPROCESS_PPID',   [CapsetId, Word32],             'Parent process ID'),
    EventType(34, 'SPARK_COUNTERS',   7*[Word64],                     'Spark counters'),
    EventType(35, 'SPARK_CREATE',     [],                             'Spark create'),
    EventType(36, 'SPARK_DUD',        [],                             'Spark dud'),
    EventType(37, 'SPARK_OVERFLOW',   [],                             'Spark overflow'),
    EventType(38, 'SPARK_RUN',        [],                             'Spark run'),
    EventType(39, 'SPARK_STEAL',      [CapNo],                        'Spark steal'),
    EventType(40, 'SPARK_FIZZLE',     [],                             'Spark fizzle'),
    EventType(41, 'SPARK_GC',         [],                             'Spark GC'),
    EventType(42, 'INTERN_STRING',    VariableLength,                 'Intern string'),
    EventType(43, 'WALL_CLOCK_TIME',  [CapsetId, Word64, Word32],     'Wall clock time'),
    EventType(44, 'THREAD_LABEL',     VariableLength,                 'Thread label'),
    EventType(45, 'CAP_CREATE',       [CapNo],                        'Create capability'),
    EventType(46, 'CAP_DELETE',       [CapNo],                        'Delete capability'),
    EventType(47, 'CAP_DISABLE',      [CapNo],                        'Disable capability'),
    EventType(48, 'CAP_ENABLE',       [CapNo],                        'Enable capability'),
    EventType(49, 'HEAP_ALLOCATED',   [CapsetId, Word64],             'Total heap memory ever allocated'),
    EventType(50, 'HEAP_SIZE',        [CapsetId, Word64],             'Current heap size (number of allocated mblocks)'),
    EventType(51, 'HEAP_LIVE',        [CapsetId, Word64],             'Current heap live data'),
    EventType(52, 'HEAP_INFO_GHC',    [CapsetId, Word16] + 4*[Word64], 'Heap static parameters'),
    EventType(53, 'GC_STATS_GHC',     [CapsetId, Word16] + 3*[Word64] + [Word32] + 3*[Word64], 'GC statistics'),
    EventType(54, 'GC_GLOBAL_SYNC',   [],                             'Synchronise stop-the-world GC'),
    EventType(55, 'TASK_CREATE',      [TaskId, CapNo, KernelThreadId],      'Task create'),
    EventType(56, 'TASK_MIGRATE',     [TaskId, CapNo, CapNo],         'Task migrate'),
    EventType(57, 'TASK_DELETE',      [TaskId],                       'Task delete'),
    EventType(58, 'USER_MARKER',      VariableLength,                 'User marker'),
    EventType(59, 'HACK_BUG_T9003',   [],                             'Empty event for bug #9003'),

    # Range 60 - 80 is used by eden for parallel tracing.
    # See http://www.mathematik.uni-marburg.de/~eden/

    EventType(90, 'MEM_RETURN',       [CapsetId, Word32, Word32, Word32],    'The RTS attempted to return heap memory to the OS'),
    EventType(91, 'BLOCKS_SIZE',      [CapsetId, Word64],                 'Report the size of the heap in blocks'),

    # Range 100 - 139 is reserved for Mercury.

    # Range 140 - 159 is reserved for Perf events.

    # Range 160 - 180 is reserved for cost-centre heap profiling events.

    # Cost-centre profiler
    EventType(160, 'HEAP_PROF_BEGIN',              VariableLength,        'Start of heap profile'),
    EventType(161, 'HEAP_PROF_COST_CENTRE',        VariableLength,        'Cost-centre definition'),
    EventType(162, 'HEAP_PROF_SAMPLE_BEGIN',       [Word64],              'Start of heap profile sample'),
    EventType(163, 'HEAP_PROF_SAMPLE_COST_CENTRE', VariableLength,        'Heap profile cost-centre sample'),
    EventType(164, 'HEAP_PROF_SAMPLE_STRING',      VariableLength,        'Heap profile string sample'),
    EventType(165, 'HEAP_PROF_SAMPLE_END',         [Word64],              'End of heap profile sample'),
    EventType(166, 'HEAP_BIO_PROF_SAMPLE_BEGIN',   [Word64, Word64],      'Start of heap profile (biographical) sample'),

    EventType(167, 'PROF_SAMPLE_COST_CENTRE',      VariableLength,        'Time profile cost-centre stack'),
    EventType(168, 'PROF_BEGIN',                   [Word64],              'Start of a time profile'),
    EventType(169, 'IPE',                          VariableLength,        'An IPE entry'),

    EventType(181, 'USER_BINARY_MSG',              VariableLength,        'User binary message'),

    # Non-moving GC
    EventType(200, 'CONC_MARK_BEGIN',              [],                    'Begin concurrent mark phase'),
    EventType(201, 'CONC_MARK_END',                [Word32],              'End concurrent mark phase'),
    EventType(202, 'CONC_SYNC_BEGIN',              [],                    'Begin concurrent GC synchronisation'),
    EventType(203, 'CONC_SYNC_END',                [],                    'End concurrent mark synchronisation'),
    EventType(204, 'CONC_SWEEP_BEGIN',             [],                    'Begin concurrent sweep phase'),
    EventType(205, 'CONC_SWEEP_END',               [],                    'End concurrent sweep phase'),
    EventType(206, 'CONC_UPD_REM_SET_FLUSH',       [CapNo],               'Update remembered set flushed'),
    EventType(207, 'NONMOVING_HEAP_CENSUS',        [Word16, Word32, Word32, Word32], 'Nonmoving heap census'),

    # Ticky-ticky profiling
    EventType(210, 'TICKY_COUNTER_DEF',            VariableLength,        'Ticky-ticky entry counter definition'),
    EventType(211, 'TICKY_COUNTER_SAMPLE',         4*[Word64],            'Ticky-ticky entry counter sample'),
    EventType(212, 'TICKY_COUNTER_BEGIN_SAMPLE',   [],                    'Ticky-ticky entry counter begin sample'),
]

def check_events() -> Dict[int, EventType]:
    seen_ids = {}
    for ty in event_types:
        if ty.n in seen_ids:
            print('Duplicate event type {n}:'.format(n=ty.n))
            print('  {name}'.format(name=ty.name))
            print('  {seen}'.format(seen=seen_ids[ty.n].name))
            assert False

        seen_ids[ty.n] = ty

    return seen_ids

def generate_event_types_array() -> str:
    x = []
    pr = lambda s: x.append(s)

    pr('/*')
    pr(' * Do not edit: This file is generated by gen_event_types.py')
    pr(' */')
    pr('')
    pr('EventType eventTypes[] = {')
    for ty in event_types:
        if ty.fields is VariableLength:
            length = '0xffff'
        elif len(ty.fields) == 0:
            length = '0'
        else:
            length = ' + '.join('sizeof({c_type})'.format(c_type=field.c_type)
                                for field in ty.fields)

        pr('    [EVENT_{}] = {{'.format(ty.name))
        pr('      .etNum = {},'.format(ty.n))
        pr('      .size = {},'.format(length))
        pr('      .desc = "{}"'.format(ty.description))
        pr('    },')

    pr('};')
    return '\n'.join(x)

def generate_event_types_defines() -> str:
    x = []
    pr = lambda s: x.append(s)

    pr('/*')
    pr(' * Do not edit: This file is generated by gen_event_types.py')
    pr(' */')
    pr('')
    pr('#pragma once')
    pr('')
    for ty in event_types:
        pr('#define EVENT_{name} {n}'.format(name=ty.name, n=ty.n))

    return '\n'.join(x)

def main() -> None:
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--event-types-array', type=argparse.FileType('w'), metavar='FILE')
    parser.add_argument('--event-types-defines', type=argparse.FileType('w'), metavar='FILE')
    args = parser.parse_args()

    check_events()

    if args.event_types_array:
        args.event_types_array.write(generate_event_types_array())

    if args.event_types_defines:
        args.event_types_defines.write(generate_event_types_defines())

if __name__ == '__main__':
    main()
