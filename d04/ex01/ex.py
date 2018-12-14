from datetime import datetime
import re as re

def str_to_tuple (s) :
    m = re.match(r"\[(.*)\] (.*)", s)
    timestamp = datetime.strptime(m.group(1), '%Y-%m-%d %H:%M')
    message = m.group(2)
    match = re.match(r"Guard #(\d+) begins shift", message)
    if match :
        value = int(match.group(1))
    elif re.match(r"wakes up", message):
        value = True
    else:
        value = False
    return timestamp, value

def get_logs (tlist) :
    logs = []
    guard = -1
    asleep_since = 0
    for timestamp, message in tlist :
        if type(message) == int :
            guard = message
        elif message == False :
            asleep_since = timestamp
        else :
            logs = logs + [(guard, asleep_since, (timestamp - asleep_since).seconds // 60)]
    return logs

def guard_list (tlist) :
    lst = []
    for guard, _, _ in tlist :
        if guard not in lst : 
            lst = lst + [guard]
    return lst

def guard_sleep_time(logs, guard) :
    t = 0
    for g, _, duration in logs :
        if guard == g :
            t = t + duration
    return t

def most_sleepy(sleep_times) :
    m = 0
    g = -1
    for guard, total in sleep_times :
        if total > m :
            m, g = total, guard
    return g

def best_time(logs, guard) :
    logs = [(start, duration) for g, start, duration in logs if guard == g]
    m_in_day = 60 * 24
    minutes = [0] * m_in_day
    for start, duration in logs :
        m_start = start.minute
        m_end = m_start + duration
        for i in range(m_start, m_end) :
            m = i if i < m_in_day else i - m_in_day
            minutes[m] += 1
    m = -1
    best = -1
    for i, s in enumerate(minutes) :
        if s > m :
            m, best = s, i
    return best, m

def best_minute_ever (logs, guards) :
    top = []
    for g in guards :
        minute, repetitions = best_time(logs, g)
        top = [(g, minute, repetitions)] + top
    g, m, r = -1, -1, -1
    for guard, minute, repetitions in top :
        if repetitions > r :
            g, m, r = guard, minute, repetitions
    return g * m

fname = 'input.txt'
with open(fname) as f:
    lines = [line for line in f]
lines.sort()

m = list(map(str_to_tuple, lines))
logs = get_logs(m)
guards = guard_list(logs)
sleep_times = [(guard, guard_sleep_time(logs, guard)) for guard in guards]
best_guard = most_sleepy(sleep_times)
best_minute = best_minute_ever(logs, guards)
print(best_minute)