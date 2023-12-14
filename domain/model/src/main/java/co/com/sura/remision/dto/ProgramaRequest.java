package co.com.sura.remision.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class ProgramaRequest {
    private String idPrograma;
    private String nombre;
    private String especialidad;
    private String profesional;
}
