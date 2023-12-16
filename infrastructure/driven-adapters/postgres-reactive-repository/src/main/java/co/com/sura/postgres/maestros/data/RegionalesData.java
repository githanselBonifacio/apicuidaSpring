package co.com.sura.postgres.maestros.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;


@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "regionales")
public class RegionalesData {
    @Id
    @Column("id_regional")
    private String id;
    private String nombre;
    private Double latitud;
    private Double longitud;
    private String direccion;
}
