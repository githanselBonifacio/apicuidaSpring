package co.com.sura.remision.entity.procedimientos;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Canalizacion {
    @JsonIgnore
    private Integer idCanalizacion;
    private String idCita;
    private String tipoCanalizacion;
    private String tipoPrestacion;
}
