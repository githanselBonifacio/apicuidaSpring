package co.com.sura.entity.remision.datosremision;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class DatosAtencionPaciente {
    @JsonIgnore
    private Integer idDatosAtencion;
    private String nombreCuidador;
    private String nombreResponsable;
    private String telefonoPaciente;
    private String celularPaciente;
    private String celularPaciente2;
    private String idRemision;
}
