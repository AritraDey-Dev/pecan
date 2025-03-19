# this needs to be at the top, what version are we building
ARG IMAGE_VERSION="latest"
ARG PARENT_IMAGE="pecan/base"

# ----------------------------------------------------------------------
# PECAN FOR MODEL BASE IMAGE
# ----------------------------------------------------------------------
FROM ${PARENT_IMAGE}:${IMAGE_VERSION}

# ----------------------------------------------------------------------
# SETUP FOR PYTHON CODE
# ----------------------------------------------------------------------
RUN apt update && apt remove -y python3-pika && apt install -y pip && pip install pika>=1.0

WORKDIR /work

# variables to store in docker image
ENV RABBITMQ_URI="amqp://guest:guest@rabbitmq:15672/%2F" \
    APPLICATION="./job.sh"

# actual application that will be executed
COPY model.py /work/
CMD ["python3", "/work/model.py"]
