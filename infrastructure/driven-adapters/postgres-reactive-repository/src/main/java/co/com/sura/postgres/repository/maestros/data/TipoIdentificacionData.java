package co.com.sura.postgres.repository.maestros.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "tipo_identificacion")
public class TipoIdentificacionData {
    @Id
    private Integer id;
    private String idTipo;
    private String nombre;
}
