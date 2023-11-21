package co.com.sura.entity.remision;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Curacion {
    @JsonIgnore
    private Integer idCuracion;
    private String idCita;
    private String tipoCuracion;
    private String descripcion;
    private Integer sesiones;
}
