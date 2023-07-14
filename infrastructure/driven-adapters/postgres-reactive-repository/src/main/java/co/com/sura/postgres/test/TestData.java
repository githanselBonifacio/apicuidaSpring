package co.com.sura.postgres.test;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
@Table("tbl_test")
public class TestData {
    @Column("id")
    @Id
    private Long id;
    @Column("name")
    private String name;
    @Column("description")
    private String description;
}
