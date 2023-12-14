package co.com.sura.remision.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class FrecuenciaRequest {
    private String idFrecuencia;
    private String tipo;
    private String descripcion;
}
