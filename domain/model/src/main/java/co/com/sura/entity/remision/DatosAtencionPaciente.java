package co.com.sura.entity.remision;

import co.com.sura.dto.remision.UbicacionRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class DatosAtencionPaciente {
    private String nombreCuidador;
    private String nombreResponsable;
    private String telefonoPaciente;
    private String celularPaciente;
    private String celularPaciente2;
    private UbicacionRequest ubicacion;
}
