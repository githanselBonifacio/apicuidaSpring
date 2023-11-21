package co.com.sura.genericos;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum Numeros {
    CIEN(100),
    SEGUNDOS_EN_HORAS(3600);

    private final int value;

}
