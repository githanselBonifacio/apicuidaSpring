package co.com.sura.entity.remision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Sondaje {
    private String tipoSondaje;
    private String tipoSonda;
    private Integer totalSesiones;
    private String tipoPrestacion;
}
