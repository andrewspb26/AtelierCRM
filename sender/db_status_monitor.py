# -*- coding: utf-8 -*-

import sqlite3
import pandas
import json
import datetime
import smtplib
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart

db = '../skiniyaCRM.db'


#-----------------------------------------------------------------------------------------
"""
1) извлекаем из json-конфига дату-время последнего запуска.
"""
with open("config.json", "r") as read_file:
     config = json.load(read_file)

last_run = config["last_run"]
now = str(datetime.datetime.now()).split('.')[0]


#-----------------------------------------------------------------------------------------
"""
2) извлекаем из таблицы eventlog все ивенты, созданные после этой даты
и по которым мы будет отправлять письмо {в производстве, отправлен}
3) по order_hash извлекаем user_name получателя, а по user_name из таблицы clients извлекаем email (2 и 3 можно объединить в один пункт- большой запрос с джойном).
"""
conn = sqlite3.connect(db)
curs = conn.cursor()

# результат запроса: order_hash, event, created_at, user_name, postal_code, email
curs.execute('select t3.eid, t3.o_hash, t3.even, t3.created, t3.u_name, t3.p_code, t4.email from (select t1.event_id as eid, t1.order_hash as o_hash, t1.event as even, t1.created_at as created, t2.user_name as u_name, t2.postal_code as p_code from (select event_id, order_hash, event, created_at from eventlog where created_at > datetime("%s") and event in ("в производстве", "отправлен")) as t1 inner join orders as t2 on t1.order_hash = t2.order_hash) t3 inner join clients t4 on t3.u_name = t4.name' % last_run)

event_list = curs.fetchall()
conn.commit()
conn.close()
# на случай, если передача сообщений по почте затянется или зависнет по каким-либо причинам,
# отключаемся от базы, чтобы она была доступна другим службам


#-----------------------------------------------------------------------------------------
"""
4) в зависимости от event'a выбираем шаблон письма, подставляем user_name (и postal_code, если ивент 'отправлен') и отправляем на соответствующий email (тут цикл по всем ивентам-покупателям).
5) записываем в eventlog order_hash'ы и события (sent_start, sent_sent) в соответствии с ивентом, по которому выбирался шаблон.
5) обновляем дату-время в json-конфиге (ставим дату-время в момент пункта 1).

конфиг должен быть следующего вида:
{"last_run":"1970-01-01 23:45:45"}
"""
#letter_pattern = 'Уважаемый %s, Ваш заказ №%s %s. '
letter_pattern = 'Dear %s, Your order №%s %s. '


for x in event_list:
        server = smtplib.SMTP(config['host'])
	# для яндекса "server = smtplib.SMTP_SSL(config['host'])"
	# и host тогда smtp.yandex.ru:465
	# если логиниться не в цикле, может появляться
        # ошибка, что слишком много писем за один раз отправлено
        server.login(config['email'], config['password'])

        name = x[4]
        order = x[1]
        status = 'sent' if x[2] == 'отправлен' else 'is in producing'
        pcode = x[5]
        email = 'email@mail.ru'
	# когда всё точно будет работать соеденить верхнюю и нижнюю строчки
        #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!x[6]
        letter = letter_pattern % (name, order, status)
        if x[2] == 'отправлен' and pcode != 'None':
                letter += 'Postal code: %s.' % pcode
        body = letter

        msg = MIMEMultipart()
        msg['From'] = config['email']
        msg['To'] = email
        msg['Subject'] = 'Статус заказа %s' % order
        msg.attach(MIMEText(body, 'html', _charset='utf-8'))
        text = msg.as_string()

        server.sendmail(config['email'], email, text)

        server.quit()

conn = sqlite3.connect(db)
curs = conn.cursor()
curs.execute('select max(event_id) from eventlog')
last_event_id = curs.fetchall()[0][0]
inserting_values = ''

for x in event_list:
        last_event_id += 1
        inserting_values += "(%s, '%s', '%s', '%s'), " % (last_event_id, x[1], 'sent', str(datetime.datetime.now()).split('.')[0])

if len(inserting_values) > 0:
        curs.execute('insert into eventlog values %s' % inserting_values[0:-2])


conn.commit()
conn.close()

config["last_run"] = now
with open("config.json", "w") as write_file:
     config = json.dump(config, write_file)
