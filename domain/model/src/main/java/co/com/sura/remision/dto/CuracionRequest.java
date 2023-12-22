package co.com.sura.remision.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class CuracionRequest {
    private String descripcion;
    private TipoCuracionRequest tipoCuracion;
    private List<DiaRequest> dias;
    private Integer sesiones;
}
