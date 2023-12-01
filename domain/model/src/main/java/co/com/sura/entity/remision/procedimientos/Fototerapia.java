package co.com.sura.entity.remision.procedimientos;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Fototerapia {
    @JsonIgnore
    private Integer idFototerapia;
    private String idCita;
    private Integer diasTratamiento;
    private String tipoFrecuencia;
    private Integer cantidadDosis;
    private String tipoPrestacion;
}
