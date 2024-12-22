FROM python:alpine3.17

WORKDIR /gains_app

COPY requirements.txt .
COPY capital_gains.py .

RUN pip install -r requirements.txt

ENV FLASK_APP=capital_gains.py

CMD ["flask", "run", "--host=0.0.0.0", "--port=8080"]
