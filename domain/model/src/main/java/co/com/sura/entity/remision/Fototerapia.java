package co.com.sura.entity.remision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Fototerapia {
    private Integer diasTratamiento;
    private String tipoFrecuencia;
    private Integer cantidadDosis;
    private String tipoPrestacion;
}
