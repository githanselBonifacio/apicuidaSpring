package co.com.sura.constantes;

import lombok.Getter;

@Getter
public enum StatusCode {
    STATUS_500(500),
    STATUS_200(200),
    STATUS_400(400);

    private final Integer value;

    StatusCode(Integer value) {
        this.value = value;
    }
}
